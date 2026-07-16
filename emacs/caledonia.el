;;; caledonia.el --- Emacs integration for Caledonia -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ryan Gibb

;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Version: 0.5.0
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1"))
;; URL: https://ryan.freumh.org/caledonia.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an Emacs interface to the Caledonia calendar CLI.
;; It communicates with Caledonia using S-expressions for data exchange.
;; The primary view is an org-agenda style agenda grouped by date.

;;; Code:

(require 'cl-lib)
(require 'calendar)
(require 'subr-x)
(require 'pulse nil t)
(require 'org)

;; Newer Emacs readers consult this switch for evaluation-capable reader forms.
;; Declaring it here also makes the dynamic safety binding work on older Emacs
;; releases whose readers reject those forms unconditionally.
(defvar read-eval nil)

(defgroup caledonia nil
  "Interface to Caledonia calendar client."
  :group 'calendar
  :prefix "caledonia-")

(defcustom caledonia-executable (executable-find "caled")
  "Path to the Caledonia executable."
  :type 'string
  :group 'caledonia)

(defcustom caledonia-server-timeout 5.0
  "Seconds to wait for a correlated server response."
  :type 'number
  :group 'caledonia)

(defcustom caledonia-server-log-limit 100000
  "Maximum characters retained in each server diagnostic buffer."
  :type 'integer
  :group 'caledonia)

(defcustom caledonia-server-frame-limit 1048576
  "Maximum characters accepted in one protocol frame."
  :type 'integer
  :group 'caledonia)

(defface caledonia-calendar-name-face
  '((t :inherit font-lock-function-name-face))
  "Face used for calendar names in the events view."
  :group 'caledonia)

(defface caledonia-date-face
  '((t :inherit font-lock-string-face))
  "Face used for dates in the events view."
  :group 'caledonia)

(defface caledonia-summary-face
  '((t :inherit default))
  "Face used for event summaries in the events view."
  :group 'caledonia)

(defface caledonia-location-face
  '((t :inherit font-lock-comment-face))
  "Face used for event locations in the events view."
  :group 'caledonia)

(defface caledonia-agenda-date-face
  '((t :inherit org-agenda-date :weight bold))
  "Face used for date headers in the agenda view."
  :group 'caledonia)

(defface caledonia-agenda-time-face
  '((t :inherit font-lock-string-face))
  "Face used for times in the agenda view."
  :group 'caledonia)

(defcustom caledonia-from-date "today"
  "Default start date for calendar view."
  :type 'string
  :group 'caledonia)

(defcustom caledonia-to-date "+3m"
  "Default end date for calendar view (3 months from today)."
  :type 'string
  :group 'caledonia)

;; Define histories for input fields

(defvar caledonia-from-history nil "History for from date inputs.")
(defvar caledonia-to-history nil "History for to date inputs.")
(defvar caledonia-timezone-history nil "History for timezone inputs.")
(defvar caledonia-calendars-history nil "History for calendar inputs.")
(defvar caledonia-text-history nil "History for search text inputs.")
(defvar caledonia-id-history nil "History for event ID inputs.")
(defvar caledonia-limit-history nil "History for limit inputs.")
(defvar caledonia-summary-history nil "History for event summary inputs.")
(defvar caledonia-location-history nil "History for event location inputs.")

;; Internal variables

(defvar caledonia--agenda-buffer "*Caledonia Agenda*"
  "Buffer name for the agenda view.")
(defvar caledonia--details-buffer "*Caledonia Event Details*"
  "Buffer name for displaying Caledonia event details.")
(defvar caledonia--server-process nil
  "The persistent Caledonia server process.")
(defvar caledonia--server-buffer-name "*caledonia-server-io*"
  "Buffer for server process I/O.")
(defvar caledonia--server-error-buffer-name "*caledonia-server-errors*"
  "Buffer for server diagnostics, kept separate from protocol stdout.")
(defvar caledonia--server-error-process nil
  "Pipe process used to keep server stderr bounded.")
(defvar caledonia--pending-responses (make-hash-table :test #'equal)
  "Responses keyed by protocol request ID.")
(defvar caledonia--request-sequence 0
  "Monotonic request sequence for this Emacs session.")
(defvar caledonia--handshake-complete nil
  "Non-nil after protocol version 1 has been negotiated.")

;; Server communication

(defvar caledonia--server-line-buffer "")

(defun caledonia--trim-log-buffer (buffer)
  "Trim BUFFER to `caledonia-server-log-limit' characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (> (buffer-size) caledonia-server-log-limit)
        (let ((inhibit-read-only t))
          (delete-region (point-min)
                         (- (point-max) caledonia-server-log-limit)))))))

(defun caledonia--server-error-filter (_process output)
  "Append server stderr OUTPUT to its bounded diagnostics buffer."
  (with-current-buffer
      (get-buffer-create caledonia--server-error-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert output)
      (caledonia--trim-log-buffer (current-buffer)))))

(defun caledonia--stop-server-error-process ()
  "Dispose of the current server stderr pipe, if any."
  (when (and caledonia--server-error-process
             (process-live-p caledonia--server-error-process))
    (delete-process caledonia--server-error-process))
  (setq caledonia--server-error-process nil))

(defun caledonia--alist-value (key fields)
  "Return KEY from protocol record FIELDS."
  (cadr (assq key fields)))

(defconst caledonia--protocol-text-fields
  '(request_id server_version message
    id summary start start_local start_utc start_tz
    end end_local end_tz location description alarms file calendar
    calendar_key source_fingerprint source_ics occurrence_start
    occurrence_timezone timezone value tzid rrule name namespace uri)
  "Protocol record fields whose atomic values are textual, not symbols.")

(defconst caledonia--protocol-text-list-fields
  '(Calendars categories_value attendees recurrence_set_value)
  "Protocol fields or constructors containing lists of textual atoms.")

(defun caledonia--protocol-atom-string (value)
  "Convert a protocol atom VALUE to a string when Emacs read it as a symbol."
  (if (symbolp value) (symbol-name value) value))

(defun caledonia--normalize-protocol-value (value)
  "Normalize textual atoms in server protocol VALUE.
Field names, constructors, booleans, and discriminators such as `kind',
`action', and `related' remain symbols.  Sexplib emits safe textual atoms
without quotes, so schema-aware conversion is required before forms use them
as Emacs strings."
  (cond
   ((atom value) value)
   ((and (symbolp (car value))
         (consp (cdr value))
         (null (cddr value)))
    (let ((key (car value))
          (field-value (cadr value)))
      (cond
       ((memq key caledonia--protocol-text-fields)
        (list key
              (if (atom field-value)
                  (caledonia--protocol-atom-string field-value)
                (caledonia--normalize-protocol-value field-value))))
       ((memq key caledonia--protocol-text-list-fields)
        (list key
              (if (listp field-value)
                  (mapcar
                   (lambda (item)
                     (if (atom item)
                         (caledonia--protocol-atom-string item)
                       (caledonia--normalize-protocol-value item)))
                   field-value)
                field-value)))
       (t (mapcar #'caledonia--normalize-protocol-value value)))))
   (t (mapcar #'caledonia--normalize-protocol-value value))))

(defun caledonia--server-filter (process output)
  "Filter PROCESS OUTPUT."
  (when (eq process caledonia--server-process)
    ;; Protocol stdout is logged separately from stderr and kept bounded.
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (insert output)
        (caledonia--trim-log-buffer (current-buffer))))
    ;; Append new output to line buffer
    (setq caledonia--server-line-buffer
          (concat caledonia--server-line-buffer output))
    ;; Bound each complete newline-delimited frame and the remaining partial
    ;; tail independently.  A process filter chunk may legitimately contain
    ;; many frames whose combined size is larger than the per-frame limit.
    (let* ((lines (split-string caledonia--server-line-buffer "\n"))
           (partial-tail (car (last lines)))
           (complete-lines (butlast lines))
           (oversized
            (or (> (length partial-tail) caledonia-server-frame-limit)
                (cl-some
                 (lambda (line)
                   (> (length line) caledonia-server-frame-limit))
                 complete-lines))))
      (if oversized
        (progn
          (setq caledonia--server-line-buffer "")
          (with-current-buffer
              (get-buffer-create caledonia--server-error-buffer-name)
            (goto-char (point-max))
            (insert "Client protocol error: response frame exceeded configured limit\n")
            (caledonia--trim-log-buffer (current-buffer)))
          (delete-process process))
        ;; Keep only the final, possibly incomplete line for the next chunk.
        (setq caledonia--server-line-buffer partial-tail)
        ;; Process every complete line; request IDs allow interleaved responses.
        (dolist (line complete-lines)
          (unless (string-empty-p line)
            (condition-case err
                (let* ((read-eval nil)
                       (parsed (read-from-string line))
                       (read-end (cdr parsed))
                       (trailing (substring line read-end))
                       (response
                        (caledonia--normalize-protocol-value (car parsed))))
                  (unless (string-match-p "\\`[[:space:]]*\\'" trailing)
                    (error "trailing data after response frame"))
                  (unless (and (listp response) (eq (car response) 'Response))
                    (error "invalid response envelope: %S" response))
                  (let* ((fields (cadr response))
                         (version (caledonia--alist-value 'version fields))
                         (request-id
                          (caledonia--alist-value 'request_id fields)))
                    (unless (equal version 1)
                      (error "unsupported protocol response version: %S"
                             version))
                    (unless (stringp request-id)
                      (error "response is missing request_id"))
                    (unless
                        (eq (gethash request-id caledonia--pending-responses)
                            :pending)
                      (error "unsolicited response request_id: %s" request-id))
                    (puthash request-id response
                             caledonia--pending-responses)))
              (error
               (with-current-buffer
                   (get-buffer-create caledonia--server-error-buffer-name)
                 (goto-char (point-max))
                 (insert (format "Client protocol error: %s\n"
                                 (error-message-string err)))
                 (caledonia--trim-log-buffer (current-buffer)))))))))))

(defun caledonia--server-sentinel (process event)
  "Listen on PROCESS for an EVENT."
  (when (eq process caledonia--server-process)
    (message "Caledonia Server process event: %s (%s)" process event)
    (setq caledonia--server-process nil
          caledonia--server-line-buffer ""
          caledonia--handshake-complete nil)
    (caledonia--stop-server-error-process)
    (clrhash caledonia--pending-responses)))

(defun caledonia--next-request-id ()
  "Return a new protocol request ID."
  (setq caledonia--request-sequence (1+ caledonia--request-sequence))
  (format "emacs-%d-%d" (emacs-pid) caledonia--request-sequence))

(defun caledonia--request-internal (request)
  "Send protocol payload REQUEST and return its successful payload."
  (let* ((request-id (caledonia--next-request-id))
         (envelope `(Request ((version 1)
                              (request_id ,request-id)
                              (request ,request))))
         (deadline (+ (float-time) caledonia-server-timeout)))
    (puthash request-id :pending caledonia--pending-responses)
    (process-send-string caledonia--server-process
                         (concat (prin1-to-string envelope) "\n"))
    (while (and (eq (gethash request-id caledonia--pending-responses) :pending)
                (< (float-time) deadline)
                (process-live-p caledonia--server-process))
      (accept-process-output caledonia--server-process 0.1))
    (let ((response (gethash request-id caledonia--pending-responses)))
      (remhash request-id caledonia--pending-responses)
      (when (eq response :pending)
        (error "Caledonia: timed out after %.1f seconds waiting for %s"
               caledonia-server-timeout request-id))
      (unless response
        (error "Caledonia: server exited while waiting for %s" request-id))
      (let* ((fields (cadr response))
             (status (caledonia--alist-value 'response fields)))
        (pcase status
          (`(Ok ,payload) payload)
          (`(Error ,error-fields)
           (let ((code (caledonia--alist-value 'code error-fields))
                 (message (caledonia--alist-value 'message error-fields)))
             (error "Caledonia [%s]: %s" code message)))
          (_ (error "Caledonia: invalid response status: %S" status)))))))

(defun caledonia--ensure-server-running ()
  "Run the caledonia binary in server mode."
  (unless (and caledonia--server-process (process-live-p caledonia--server-process))
    (message "Caledonia  Starting server...")
    (setq caledonia--server-line-buffer ""
          caledonia--handshake-complete nil)
    (clrhash caledonia--pending-responses)
    (caledonia--stop-server-error-process)
    (setq caledonia--server-error-process
          (make-pipe-process
           :name "caledonia-server-stderr"
           :buffer nil
           :filter #'caledonia--server-error-filter
           :coding 'utf-8-unix
           :noquery t))
    (setq caledonia--server-process
          (make-process
           :name "caledonia-server"
           :buffer (get-buffer-create caledonia--server-buffer-name)
           :stderr caledonia--server-error-process
           :command (list caledonia-executable "server")
           :connection-type 'pipe
           :noquery t))
    (unless (and caledonia--server-process (process-live-p caledonia--server-process))
      (error "Caledonia  Failed to start server process"))
    (set-process-filter caledonia--server-process #'caledonia--server-filter)
    (set-process-sentinel caledonia--server-process #'caledonia--server-sentinel)
    (let ((hello (caledonia--request-internal 'Handshake)))
      (unless (and (listp hello) (eq (car hello) 'Hello)
                   (equal (caledonia--alist-value
                           'protocol_version (cadr hello))
                          1))
        (delete-process caledonia--server-process)
        (error "Caledonia: protocol handshake failed: %S" hello))
      (setq caledonia--handshake-complete t))
    (message "Caledonia  Server started.")))

(defun caledonia--send-request (request)
  "Send protocol payload REQUEST and return its successful payload."
  (caledonia--ensure-server-running)
  (unless caledonia--handshake-complete
    (error "Caledonia: server handshake is incomplete"))
  (caledonia--request-internal request))

(defun caledonia--get-events (event-payload)
  "Parse EVENT-PAYLOAD of structure (Events (events...))."
  (if (and (listp event-payload) (eq (car event-payload) 'Events))
      (let ((event-list (cadr event-payload)))
        event-list)
    (error "Caledonia: invalid Events payload: %S" event-payload)))

;; Helper functions

(defun caledonia--format-timestamp (iso-string &optional format)
  "Format ISO-8601 time string ISO-STRING to human-readable format.
FORMAT defaults to \"%Y-%m-%d %H:%M\" if not specified.
The time string is assumed to already be in the correct timezone
\(the server sends times pre-converted\), so we encode and format
in UTC to avoid any local timezone conversion."
  (let* ((parsed (parse-time-string iso-string))
         (time (apply #'encode-time
                      (append (cl-subseq parsed 0 6) (list nil t 0)))))
    (format-time-string (or format "%Y-%m-%d %H:%M") time t)))

(defun caledonia--get-key (key event)
  "Get KEY from EVENT as a string."
  (let ((value (cadr (assoc key event))))
    (cond
     ((null value) nil)
     ((stringp value) value)
     ((symbolp value) (symbol-name value))
     (t value))))

(defun caledonia--protocol-time-display (time)
  "Return editable display text for structured protocol TIME."
  (when time
    (let ((value (caledonia--alist-value 'value time)))
      (and value (replace-regexp-in-string "T" " " value t t)))))

(defun caledonia--protocol-timezone-display (time)
  "Return timezone form text for structured protocol TIME."
  (when time
    (pcase (caledonia--alist-value 'kind time)
      ('utc "UTC")
      ('floating "FLOATING")
      ('tzid (caledonia--alist-value 'tzid time))
      (_ nil))))

(defun caledonia--protocol-end-time (end)
  "Return the calendar time nested in structured END, if it is DTEND."
  (when (and end (eq (caledonia--alist-value 'kind end) 'dtend))
    (caledonia--alist-value 'value end)))

(defun caledonia--protocol-end-display (end)
  "Return editable form text for structured END, including DURATION."
  (when end
    (pcase (caledonia--alist-value 'kind end)
      ('dtend
       (caledonia--protocol-time-display
        (caledonia--alist-value 'value end)))
      ('duration
       (format "duration:%s" (caledonia--alist-value 'seconds end)))
      (_ nil))))

(defun caledonia--find-and-highlight-event-in-file (file event-id)
  "Find EVENT-ID in FILE, position cursor, and highlight the event.
Return non-nil if the event was found."
  (when (and file event-id)
    (let ((id-str (format "%s" event-id))
          (found nil))
      ;; Try to find and highlight iCalendar VEVENT block
      (goto-char (point-min))
      (when (and (string-match-p "\\.ics$" file)
                 (search-forward (format "UID:%s" id-str) nil t))
        ;; Found the UID in an ICS file, try to highlight the VEVENT block
        (let ((uid-pos (match-beginning 0))
              (vevent-start nil)
              (vevent-end nil))
          ;; Find start of the VEVENT block
          (save-excursion
            (goto-char uid-pos)
            (if (search-backward "BEGIN:VEVENT" nil t)
                (setq vevent-start (match-beginning 0))
              (setq vevent-start uid-pos)))
          ;; Find end of the VEVENT block
          (save-excursion
            (goto-char uid-pos)
            (if (search-forward "END:VEVENT" nil t)
                (setq vevent-end (match-end 0))
              (setq vevent-end (line-end-position))))
          ;; Highlight the whole VEVENT block if found
          (when (and vevent-start vevent-end)
            (goto-char vevent-start)
            (caledonia--highlight-region vevent-start vevent-end)
            (recenter)
            (setq found t))))
      (unless found
        (message "Event ID not found in file"))
      found)))

(defun caledonia--display-event-details (event)
  "Display details for EVENT in a separate buffer."
  (let ((buf (get-buffer-create caledonia--details-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (let* ((summary (caledonia--get-key 'summary event))
               (calendar (caledonia--get-key 'calendar event))
               (start (caledonia--get-key 'start event))
               (end (caledonia--get-key 'end event))
               (start-tz (caledonia--get-key 'start_tz event))
               (end-tz (caledonia--get-key 'end_tz event))
               (is-date (caledonia--get-key 'is_date event))
               (recurring (caledonia--get-key 'recurring event))
               (alarms (caledonia--get-key 'alarms event))
               (location (caledonia--get-key 'location event))
               (description (caledonia--get-key 'description event))
               (file (caledonia--get-key 'file event))
               (start-fmt (when start
                            (if is-date
                                (caledonia--format-timestamp start "%Y-%m-%d")
                              (caledonia--format-timestamp start "%Y-%m-%d %H:%M"))))
               (end-fmt (when end
                          (if is-date
                              (caledonia--format-timestamp end "%Y-%m-%d")
                            (caledonia--format-timestamp end "%Y-%m-%d %H:%M")))))
          (when calendar
            (insert (propertize "Calendar: " 'face 'bold) calendar "\n"))
          (when summary
            (insert (propertize "Summary: " 'face 'bold) summary "\n"))
          (when start-fmt
            (insert (propertize "Start: " 'face 'bold) start-fmt
                    (if start-tz (format " (%s)" start-tz) "") "\n"))
          (when end-fmt
            (insert (propertize "End: " 'face 'bold) end-fmt
                    (if end-tz (format " (%s)" end-tz) "") "\n"))
          (when recurring
            (insert (propertize "Recurring: " 'face 'bold) "yes\n"))
          (when alarms
            (insert (propertize "Alarms: " 'face 'bold) alarms "\n"))
          (when location
            (insert (propertize "Location: " 'face 'bold) location "\n"))
          (when description
            (insert (propertize "Description: " 'face 'bold) description "\n"))
          (when file
            (insert "\n" (propertize "File: " 'face 'bold)
                    (propertize file 'face 'link
                                'mouse-face 'highlight
                                'help-echo "Click to open file"
                                'keymap (let ((map (make-sparse-keymap))
                                              (event-copy event))
                                          (define-key map [mouse-1]
                                                      (lambda ()
                                                        (interactive)
                                                        (find-file file)
                                                        (caledonia--find-and-highlight-event-in-file
                                                         file (caledonia--get-key 'id event-copy))))
                                          (define-key map (kbd "RET")
                                                      (lambda ()
                                                        (interactive)
                                                        (find-file file)
                                                        (caledonia--find-and-highlight-event-in-file
                                                         file (caledonia--get-key 'id event-copy))))
                                          map))
                    "\n")))))
    (switch-to-buffer-other-window buf)))

(defun caledonia--highlight-region (start end)
  "Highlight the region between START and END."
  (when (fboundp 'pulse-momentary-highlight-region)
    (pulse-momentary-highlight-region start end))
  ;; Fallback for when pulse is not available
  (unless (fboundp 'pulse-momentary-highlight-region)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face 'highlight)
      (run-with-timer 0.5 nil (lambda () (delete-overlay overlay))))))

;; Input helpers

(defun caledonia--read-date-range ()
  "Read a date range from the user with `org-mode' date picker integration.
Returns a cons cell (from-date . to-date).
The from-date can be nil to indicate no start date constraint."
  (let (from to)
    (setq from
          (if (y-or-n-p "Set a start date? ")
              (org-read-date nil nil nil "From date: " nil nil t)
                                        ; empty string differentiates from nil for optional args later on
            ""))
    ;; Use org-mode's date picker for To date (must have a value)
    (setq to (org-read-date nil nil nil "To date: " nil nil t))
    (cons from to)))

(defun caledonia--get-available-calendars ()
  "Get list of available calendar names from server."
  (let ((response (caledonia--send-request 'ListCalendars)))
    (if (and (listp response) (eq (car response) 'Calendars))
        (cadr response)
      nil)))

(defun caledonia--sexp-escape-string (s)
  "Escape S for sexp serialization, handling newlines and backslashes."
  (let ((escaped (replace-regexp-in-string "\\\\" "\\\\\\\\" s)))
    (setq escaped (replace-regexp-in-string "\n" "\\\\n" escaped))
    (setq escaped (replace-regexp-in-string "\"" "\\\\\"" escaped))
    (format "\"%s\"" escaped)))

(defun caledonia--sexp-field (key value)
  "Format KEY VALUE pair as sexp field string, or empty string if VALUE is nil."
  (cond
   ((null value) "")
   ((listp value)
    (format "(%s (%s))" key (mapconcat (lambda (s) (caledonia--sexp-escape-string s)) value " ")))
   (t (format "(%s %s)" key (caledonia--sexp-escape-string value)))))

(defun caledonia--build-sexp-fields (fields)
  "Build sexp string from alist FIELDS, omitting nil values."
  (let ((parts (cl-remove-if #'string-empty-p
                              (mapcar (lambda (pair)
                                        (caledonia--sexp-field (car pair) (cdr pair)))
                                      fields))))
    (mapconcat #'identity parts " ")))

;; Agenda view

(defvar caledonia-agenda-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'caledonia-show-event)
    (define-key map (kbd "M-RET") 'caledonia-open-event-file)
    (define-key map (kbd "r") 'caledonia-refresh)
    (define-key map (kbd "a") 'caledonia-add-event)
    (define-key map (kbd "e") 'caledonia-edit-event)
    (define-key map (kbd "d") 'caledonia-delete-event)
    (define-key map (kbd "s") 'caledonia-search)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'caledonia-agenda-help)
    map)
  "Keymap for Caledonia agenda mode.")

(defvar caledonia-agenda--help-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'caledonia-show-event)
    (define-key map (kbd "M-RET") 'caledonia-open-event-file)
    (define-key map (kbd "r") 'caledonia-refresh)
    (define-key map (kbd "a") 'caledonia-add-event)
    (define-key map (kbd "e") 'caledonia-edit-event)
    (define-key map (kbd "d") 'caledonia-delete-event)
    (define-key map (kbd "s") 'caledonia-search)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Clean keymap for which-key help display (no inherited bindings).")

(defun caledonia-agenda-help ()
  "Show available keybindings."
  (interactive)
  (if (fboundp 'which-key-show-keymap)
      (which-key-show-keymap 'caledonia-agenda--help-map)
    (describe-mode)))

(define-derived-mode caledonia-agenda-mode special-mode "Caledonia-Agenda"
  "Major mode for displaying calendar events in an agenda view.")

(defvar-local caledonia-agenda--query nil
  "Current query used by this agenda buffer.")

(defvar-local caledonia-agenda--from-date nil
  "Start date as (year month day) for the agenda range.")

(defvar-local caledonia-agenda--to-date nil
  "End date as (year month day) for the agenda range.")

(defun caledonia--parse-iso-date (iso-string)
  "Parse ISO-STRING and return (year month day hour minute).
Extracts components directly without timezone conversion."
  (unless (and
           (stringp iso-string)
           (string-match
            (rx string-start
                (group (= 4 digit)) "-"
                (group (= 2 digit)) "-"
                (group (= 2 digit))
                (opt "T"
                     (group (= 2 digit)) ":"
                     (group (= 2 digit))
                     (opt ":" (= 2 digit) (opt "." (+ digit)))
                     (opt (or "Z"
                              (seq (any "+-") (= 2 digit) ":"
                                   (= 2 digit)))))
                string-end)
            iso-string))
    (error "Invalid ISO calendar date: %S" iso-string))
  (list (string-to-number (match-string 1 iso-string))
        (string-to-number (match-string 2 iso-string))
        (string-to-number (match-string 3 iso-string))
        (string-to-number (or (match-string 4 iso-string) "0"))
        (string-to-number (or (match-string 5 iso-string) "0"))))

(defun caledonia--format-day-header (year month day)
  "Format a day header like \"Monday     10 March 2025\" from YEAR, MONTH, DAY."
  (let* ((time (encode-time 0 0 12 day month year nil -1))
         (dow (format-time-string "%A" time))
         (month-name (format-time-string "%B" time)))
    (format "%-10s %2d %s %d" dow day month-name year)))

(defun caledonia--date-to-absolute (year month day)
  "Convert YEAR MONTH DAY to an absolute day number for iteration."
  (calendar-absolute-from-gregorian (list month day year)))

(defun caledonia--absolute-to-date (abs)
  "Convert absolute day number ABS to (year month day)."
  (let ((greg (calendar-gregorian-from-absolute abs)))
    (list (nth 2 greg) (nth 0 greg) (nth 1 greg))))

(defun caledonia--render-agenda (events &optional from-date to-date)
  "Render EVENTS in agenda format, grouped by date.
Shows all days between FROM-DATE and TO-DATE, including empty days.
FROM-DATE and TO-DATE are (year month day) lists.  When nil, they are
derived from events."
  (let ((day-groups (make-hash-table :test 'equal)))
    ;; Group events by date (multi-day events appear on each day they span)
    ;; Use local times for date grouping so events appear on the correct local day
    (dolist (event events)
      (let* ((start (or (caledonia--get-key 'start_local event)
                        (caledonia--get-key 'start event)))
             (end-val (or (caledonia--get-key 'end_local event)
                          (caledonia--get-key 'end event)))
             (start-parsed (caledonia--parse-iso-date start))
             (start-abs (caledonia--date-to-absolute (nth 0 start-parsed) (nth 1 start-parsed) (nth 2 start-parsed)))
             (end-abs (if end-val
                          (let ((end-parsed (caledonia--parse-iso-date end-val)))
                            (caledonia--date-to-absolute (nth 0 end-parsed) (nth 1 end-parsed) (nth 2 end-parsed)))
                        start-abs)))
        ;; For date events, end is exclusive (e.g. Mar 16-17 means just Mar 16)
        ;; For timed events, include the end day
        (let ((last-abs (if (caledonia--get-key 'is_date event)
                            (1- end-abs)
                          end-abs)))
          (cl-loop for abs from start-abs to (max start-abs last-abs)
                   for date-key = (caledonia--absolute-to-date abs)
                   do (puthash date-key
                               (append (gethash date-key day-groups) (list event))
                               day-groups)))))
    ;; Find date range
    (when (or events (and from-date to-date))
      (let* ((first-event (car events))
             (last-event (car (last events)))
             (first-parsed
              (when first-event
                (caledonia--parse-iso-date
                 (or (caledonia--get-key 'start_local first-event)
                     (caledonia--get-key 'start first-event)))))
             (last-parsed
              (when last-event
                (caledonia--parse-iso-date
                 (or (caledonia--get-key 'start_local last-event)
                     (caledonia--get-key 'start last-event)))))
             (range-start
              (or from-date
                  (list (nth 0 first-parsed) (nth 1 first-parsed)
                        (nth 2 first-parsed))))
             (range-end
              (or to-date
                  (list (nth 0 last-parsed) (nth 1 last-parsed)
                        (nth 2 last-parsed))))
             (start-abs (caledonia--date-to-absolute (nth 0 range-start) (nth 1 range-start) (nth 2 range-start)))
             (end-abs (caledonia--date-to-absolute (nth 0 range-end) (nth 1 range-end) (nth 2 range-end))))
        ;; Iterate over every day in the range
        (cl-loop for abs from start-abs to end-abs
                 for date-key = (caledonia--absolute-to-date abs)
                 for year = (nth 0 date-key)
                 for month = (nth 1 date-key)
                 for day = (nth 2 date-key)
                 for day-events = (gethash date-key day-groups)
                 do
        ;; Insert date header
        (insert (propertize (caledonia--format-day-header year month day)
                            'face 'caledonia-agenda-date-face)
                "\n")
        ;; Insert events for this day (all-day events first, then by time)
        (dolist (event (sort (copy-sequence day-events)
                             (lambda (a b)
                               (let ((a-date (caledonia--get-key 'is_date a))
                                     (b-date (caledonia--get-key 'is_date b)))
                                 (cond
                                  ((and a-date (not b-date)) t)
                                  ((and (not a-date) b-date) nil)
                                  (t (string< (or (caledonia--get-key 'start_local a)
                                                  (caledonia--get-key 'start a) "")
                                              (or (caledonia--get-key 'start_local b)
                                                  (caledonia--get-key 'start b) ""))))))))
          (let* ((start (or (caledonia--get-key 'start_local event)
                            (caledonia--get-key 'start event)))
                 (end-val (or (caledonia--get-key 'end_local event)
                              (caledonia--get-key 'end event)))
                 (summary (or (caledonia--get-key 'summary event) "(no summary)"))
                 (calendar (or (caledonia--get-key 'calendar event) ""))
                 (location (caledonia--get-key 'location event))
                 (is-date (caledonia--get-key 'is_date event))
                 (start-parsed (caledonia--parse-iso-date start))
                 (start-time-str (if is-date
                                     "          "
                                   (format "%02d:%02d" (nth 3 start-parsed) (nth 4 start-parsed))))
                 (end-time-str (when (and end-val (not is-date))
                                 (let ((end-parsed (caledonia--parse-iso-date end-val)))
                                   (format "%02d:%02d" (nth 3 end-parsed) (nth 4 end-parsed)))))
                 (time-str (if is-date
                               "           "
                             (if end-time-str
                                 (format "%s-%s" start-time-str end-time-str)
                               (format "%s     " start-time-str))))
                 (location-str (if location (format " @ %s" location) ""))
                 (line (format "  %-12s %s  %s%s\n"
                               (propertize (concat calendar ":") 'face 'caledonia-calendar-name-face)
                               (propertize time-str 'face 'caledonia-agenda-time-face)
                               (propertize summary 'face 'caledonia-summary-face)
                               (propertize location-str 'face 'caledonia-location-face))))
            (insert (propertize line 'event-data event)))))))))

;; Agenda commands

(defun caledonia-show-event ()
  "Show details for the event on the current line."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (if event
        (caledonia--display-event-details event)
      (message "No event on this line"))))

(defun caledonia-open-event-file ()
  "Open the file for the event on the current line."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (if event
        (let ((file (caledonia--get-key 'file event))
              (event-id (caledonia--get-key 'id event)))
          (cond
           ((not file) (message "No file associated with this event"))
           ((not (file-exists-p file)) (message "File does not exist: %s" file))
           (t (find-file file)
              (caledonia--find-and-highlight-event-in-file file event-id))))
      (message "No event on this line"))))

(defun caledonia-refresh (&optional new-range)
  "Refresh the agenda view, reloading events from disk.
With prefix arg NEW-RANGE, prompt for a new date range."
  (interactive "P")
  (when (eq major-mode 'caledonia-agenda-mode)
    (caledonia--send-request 'Refresh)
    (let* ((query (if new-range
                      (let* ((dates (caledonia--read-date-range))
                             (from (car dates))
                             (to (cdr dates))
                             (q `((to ,to))))
                        (when (and from (not (string-empty-p from)))
                          (setq q (append q `((from ,from)))))
                        ;; Preserve non-date fields (e.g. text search)
                        (dolist (pair caledonia-agenda--query)
                          (unless (memq (car pair) '(from to))
                            (setq q (append q (list pair)))))
                        q)
                    caledonia-agenda--query))
           (payload (caledonia--send-request `(Query ,query)))
           (events (caledonia--get-events payload))
           (from-date (caledonia--resolve-date-to-ymd (cadr (assq 'from query))))
           (to-date (caledonia--resolve-date-to-ymd (cadr (assq 'to query)))))
      (setq-local caledonia-agenda--query query)
      (setq-local caledonia-agenda--from-date from-date)
      (setq-local caledonia-agenda--to-date to-date)
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (caledonia--render-agenda events from-date to-date)
        (goto-char (min pos (point-max)))))))

(defun caledonia--resolve-date-to-ymd (date-str)
  "Resolve DATE-STR (like \"today\", \"+3m\", \"2025-04-01\") to (year month day).
Uses current time for relative dates."
  (when (and date-str (not (string-empty-p date-str)))
    (let* ((time (org-read-date nil t date-str))
           (decoded (decode-time time)))
      (list (nth 5 decoded) (nth 4 decoded) (nth 3 decoded)))))

;; Timezone completion

(defvar caledonia--timezone-list nil
  "Cached list of IANA timezone names.")

(defun caledonia--timezone-list ()
  "Return a list of IANA timezone names from the system zoneinfo database."
  (or caledonia--timezone-list
      (setq caledonia--timezone-list
            (let ((zoneinfo-dir (cl-find-if #'file-directory-p
                                            '("/etc/zoneinfo"
                                              "/usr/share/zoneinfo"
                                              "/usr/lib/zoneinfo"
                                              "/usr/share/lib/zoneinfo"))))
              (when zoneinfo-dir
                (let ((metadata
                       (rx string-start
                           (or "+VERSION" "leapseconds" "localtime"
                               "posixrules" "tzdata.zi"
                               (seq "iso3166.tab" string-end)
                               (seq "zone" (opt "1970") ".tab" string-end)
                               (seq "leap" (* nonl) ".list" string-end)))))
                  (sort
                   (cl-loop
                    for file in
                    (directory-files-recursively
                     zoneinfo-dir "." nil
                     (lambda (dir)
                       (not (member (file-name-nondirectory
                                     (directory-file-name dir))
                                    '("posix" "right" "SystemV")))))
                    for relative = (file-relative-name file zoneinfo-dir)
                    unless (or (file-directory-p file)
                               (string-match-p metadata relative))
                    collect relative)
                   #'string<)))))))

;; Event form buffer

(defvar caledonia-event-form-buffer "*Caledonia Event*"
  "Buffer name for the event form.")

(defvar-local caledonia-event-form--type nil
  "Type of form: `create' or `edit'.")

(defvar-local caledonia-event-form--id nil
  "Event ID when editing.")

(defvar-local caledonia-event-form--calendar-key nil
  "Stable calendar directory key when editing.")

(defvar-local caledonia-event-form--file nil
  "Physical source file identity when editing.")

(defvar-local caledonia-event-form--source-fingerprint nil
  "Source fingerprint captured when the edit form was opened.")

(defvar-local caledonia-event-form--original nil
  "Original form strings used to construct explicit Keep/Clear/Set patches.")

(defvar-local caledonia-event-form--return-buffer nil
  "Buffer to return to after form submission.")

(defvar-local caledonia-event-form--occurrence-start nil
  "Exact RFC 3339 recurrence identity when editing one occurrence.")

(defvar-local caledonia-event-form--occurrence-timezone nil
  "Query timezone used to interpret the selected recurrence identity.")

(defvar caledonia-event-form--date-fields '("Start" "End")
  "Field names that should use org-read-date.")

(defvar caledonia-event-form--completing-fields '("Timezone" "End Timezone")
  "Field names that should use completing-read.")

(defvar caledonia-event-form-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") 'caledonia-event-form-submit)
    (define-key map (kbd "C-c C-k") 'caledonia-event-form-cancel)
    (define-key map (kbd "C-c C-d") 'caledonia-event-form-pick-date)
    (define-key map (kbd "TAB") 'caledonia-event-form-next-field)
    (define-key map (kbd "<backtab>") 'caledonia-event-form-prev-field)
    (define-key map (kbd "RET") 'caledonia-event-form-newline)
    map)
  "Keymap for Caledonia event form mode.")

(defun caledonia-event-form-newline ()
  "Insert a newline in the Description field, or move to next field otherwise."
  (interactive)
  (if (string= (caledonia-event-form--current-field) "Description")
      (newline)
    (caledonia-event-form-next-field)))

(define-derived-mode caledonia-event-form-mode text-mode "Caledonia-Event"
  "Major mode for editing calendar event fields.
\\<caledonia-event-form-mode-map>
\\[caledonia-event-form-submit] to submit, \\[caledonia-event-form-cancel] to cancel.
TAB to next field (opens org-read-date on date fields), S-TAB to previous field.")

(defun caledonia-event-form--insert-field (name &optional value read-only-value)
  "Insert a form field with NAME as read-only label and VALUE as editable.
If NAME is \"Description\", the field supports multiple lines.  When
READ-ONLY-VALUE is non-nil, present VALUE as immutable identity metadata."
  (insert (propertize (format "%s: " name)
                        'read-only t
                        'front-sticky '(read-only)
                        'rear-nonsticky '(read-only face)
                        'face 'bold
                        'field-name name))
  (let ((value (or value "")))
    (when read-only-value
      (setq value
            (propertize value 'read-only t 'front-sticky '(read-only)
                        'rear-nonsticky '(read-only face) 'face 'shadow)))
    (if (string= name "Description")
        (insert value "\n")
      (insert value)
    (insert (propertize "\n" 'read-only t
                        'front-sticky nil
                          'rear-nonsticky '(read-only))))))

(defun caledonia-event-form--insert-help ()
  "Insert the help text at the bottom of the form."
  (insert (propertize "\n" 'read-only t
                      'rear-nonsticky '(read-only)))
  (let ((help (concat
               (propertize "TAB" 'face 'bold) " next field  "
               (propertize "S-TAB" 'face 'bold) " prev field  "
               (propertize "C-c C-c" 'face 'bold) " submit  "
               (propertize "C-c C-k" 'face 'bold) " cancel")))
    (insert (propertize help 'read-only t))))

(defun caledonia-event-form--get-field (name)
  "Get the value of field NAME from the form buffer.
For the Description field, captures multiple lines up to the help text."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^%s: " (regexp-quote name)) nil t)
      (let* ((value-start (point))
             (value-end (if (string= name "Description")
                            ;; Capture everything until the read-only help separator
                            (let ((pos value-start))
                              (while (and (< pos (point-max))
                                          (not (get-text-property pos 'read-only)))
                                (setq pos (1+ pos)))
                              pos)
                          (line-end-position)))
             (raw (buffer-substring-no-properties value-start value-end))
             (val
              (cond
               ((string= name "Description")
                ;; `caledonia-event-form--insert-field' adds exactly one
                ;; editable separator newline.  Remove that newline while
                ;; preserving every newline/space authored in the value.
                (if (and (> (length raw) 0)
                         (= (aref raw (1- (length raw))) ?\n))
                    (substring raw 0 -1)
                  raw))
               ((member name '("Summary" "Location")) raw)
               (t (string-trim raw)))))
        (unless (string-empty-p val) val)))))

(defun caledonia-event-form--current-field ()
  "Return the field name on the current line, or nil.
For multi-line fields like Description, walks backwards to find the label."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (if (string-match "^\\([^:]+\\): " line)
        (match-string 1 line)
      ;; On a continuation line — walk backwards to find the field label
      (save-excursion
        (while (and (not (bobp))
                    (let ((l (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))))
                      (not (string-match "^\\([^:]+\\): " l))))
          (forward-line -1))
        (let ((l (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([^:]+\\): " l)
            (match-string 1 l)))))))

(defun caledonia-event-form--goto-field-value ()
  "Move point to the value portion of the current field line."
  (beginning-of-line)
  (when (re-search-forward "^[^:]+: " (line-end-position) t)
    (point)))

(defun caledonia-event-form--on-label-line-p ()
  "Return non-nil if the current line has a field label (Name: ...)."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (string-match-p "^[^:]+: " line)))

(defun caledonia-event-form-next-field ()
  "Move to the next field.  On date fields, open org-read-date."
  (interactive)
  (let ((start-field (caledonia-event-form--current-field)))
    (forward-line 1)
    ;; Skip continuation lines of the current field and non-field lines
    (while (and (not (eobp))
                (let ((f (caledonia-event-form--current-field)))
                  (or (not f)
                      (and (equal f start-field)
                           (not (caledonia-event-form--on-label-line-p))))))
      (forward-line 1))
    (when (caledonia-event-form--on-label-line-p)
      (caledonia-event-form--goto-field-value)
      (let ((field (caledonia-event-form--current-field)))
        (cond
         ((member field caledonia-event-form--date-fields)
          (caledonia-event-form-pick-date))
         ((member field caledonia-event-form--completing-fields)
          (caledonia-event-form-pick-completing)))))))

(defun caledonia-event-form-prev-field ()
  "Move to the previous field.  On date fields, open org-read-date."
  (interactive)
  (let ((start-field (caledonia-event-form--current-field)))
    ;; Move up past current field's label line
    (forward-line -1)
    (while (and (not (bobp))
                (let ((f (caledonia-event-form--current-field)))
                  (or (not f)
                      (and (equal f start-field)
                           (not (caledonia-event-form--on-label-line-p))))))
      (forward-line -1))
    ;; If we landed on the same field's label, go up one more field
    (when (and (equal (caledonia-event-form--current-field) start-field)
               (caledonia-event-form--on-label-line-p)
               (not (bobp)))
      (forward-line -1)
      (while (and (not (bobp))
                  (not (caledonia-event-form--on-label-line-p)))
        (forward-line -1)))
    (when (caledonia-event-form--on-label-line-p)
      (caledonia-event-form--goto-field-value)
      (let ((field (caledonia-event-form--current-field)))
        (cond
         ((member field caledonia-event-form--date-fields)
          (caledonia-event-form-pick-date))
         ((member field caledonia-event-form--completing-fields)
          (caledonia-event-form-pick-completing)))))))

(defun caledonia-event-form-pick-completing ()
  "Use completing-read for the field at point (e.g. timezone)."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line (buffer-substring-no-properties line-start line-end)))
    (when (string-match "^\\([^:]+\\): \\(.*\\)$" line)
      (let* ((field (match-string 1 line))
             (current (string-trim (match-string 2 line)))
             (candidates (pcase field
                           ("Timezone" (caledonia--timezone-list))
                           ("End Timezone" (caledonia--timezone-list))
                           (_ nil)))
             (new-val (completing-read (format "%s: " field)
                                       candidates nil nil
                                       (unless (string-empty-p current) current)
                                       'caledonia-timezone-history)))
        (let ((inhibit-read-only t))
          (delete-region line-start (min (1+ line-end) (point-max)))
          (caledonia-event-form--insert-field field new-val)
          (forward-line -1)
          (caledonia-event-form--goto-field-value))))))

(defun caledonia-event-form-pick-date ()
  "Use org-read-date to pick a date for the field at point."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line (buffer-substring-no-properties line-start line-end)))
    (when (string-match "^\\([^:]+\\): \\(.*\\)$" line)
      (let* ((field (match-string 1 line))
             (current (string-trim (match-string 2 line)))
             (default (unless (string-empty-p current) current))
             (new-val (org-read-date nil nil nil (format "%s: " field) nil default t)))
        (let ((inhibit-read-only t))
          (delete-region line-start (min (1+ line-end) (point-max)))
          (caledonia-event-form--insert-field field new-val)
          (forward-line -1)
          (caledonia-event-form--goto-field-value))))))

(defun caledonia-event-form--parse-datetime (str)
  "Parse STR as \"YYYY-MM-DD HH:MM:SS\" or \"YYYY-MM-DD\".
Returns (date . time) where time may be nil."
  (when str
    (let ((parts (split-string str " ")))
      (cons (car parts)
            (when (and (cdr parts) (string-match-p "^[0-9][0-9]:[0-9][0-9]" (cadr parts)))
              (cadr parts))))))

(defun caledonia-event-form--time-input (text timezone)
  "Build a structured protocol calendar time from TEXT and TIMEZONE."
  (let* ((parsed (caledonia-event-form--parse-datetime text))
         (date (car parsed))
         (time (cdr parsed)))
    (unless date
      (user-error "A date is required"))
    (if (not time)
        `((kind Date) (value ,date))
      (let ((value (concat date "T" time))
            (timezone (or timezone "")))
        (cond
         ((string= timezone "UTC") `((kind Utc) (value ,value)))
         ((or (string-empty-p timezone) (string= timezone "FLOATING"))
          `((kind Floating) (value ,value)))
         (t `((kind (Tzid ,timezone)) (value ,value))))))))

(defun caledonia-event-form--patch (field value &optional transform)
  "Return an explicit patch for FIELD's VALUE compared with the original.
Apply TRANSFORM to non-empty Set values."
  (let ((original (cdr (assoc field caledonia-event-form--original))))
    (cond
     ((equal value original) 'Keep)
     ((null value) 'Clear)
     (t `(Set ,(if transform (funcall transform value) value))))))

(defun caledonia-event-form--time-patch (field timezone-field value timezone)
  "Return a time patch using FIELD, TIMEZONE-FIELD, VALUE and TIMEZONE."
  (if (and (equal value (cdr (assoc field caledonia-event-form--original)))
           (equal timezone
                  (cdr (assoc timezone-field caledonia-event-form--original))))
      'Keep
    (if (null value)
        'Clear
      `(Set ,(caledonia-event-form--time-input value timezone)))))

(defun caledonia-event-form--end-patch (value timezone)
  "Return an explicit event-end patch for VALUE and TIMEZONE."
  (if (and (equal value (cdr (assoc "End" caledonia-event-form--original)))
           (equal timezone
                  (cdr (assoc "End Timezone"
                              caledonia-event-form--original))))
      'Keep
    (if (null value)
        'Clear
      `(Set ,(caledonia-event-form--end-input value timezone)))))

(defun caledonia-event-form--recurrence-patch
    (rrule clear-recurrence occurrence-p)
  "Build an explicit recurrence patch from RRULE and CLEAR-RECURRENCE.
OCCURRENCE-P forces Keep because recurrence belongs to the series master."
  (if occurrence-p
      'Keep
    (let ((clear
           (and clear-recurrence
                (downcase (string-trim clear-recurrence)))))
      (cond
       ((member clear '("yes" "y" "true" "1")) 'Clear)
       ((and clear (not (string-empty-p clear)))
        (user-error "Clear Recurrence must be yes or left blank"))
       (t
        (let ((patch (caledonia-event-form--patch "Recurrence" rrule)))
          (pcase patch
            (`(Set ,value) `(Set ((rrule ,value))))
            (_ patch))))))))

(defun caledonia-event-form--end-input (value timezone)
  "Build a DTEND or DURATION protocol value from form VALUE and TIMEZONE."
  (if (string-match "\\`duration:\\([0-9]+\\)\\'" value)
      (let ((seconds (string-to-number (match-string 1 value))))
        (when (or (<= seconds 0) (and timezone (not (string-empty-p timezone))))
          (user-error
           "Duration must be positive seconds and cannot have an end timezone"))
        `(Duration_seconds ,seconds))
    `(Dtend ,(caledonia-event-form--time-input value timezone))))

(defun caledonia-event-form--all-patches-keep-p (fields)
  "Return non-nil when every editable patch in protocol FIELDS is Keep."
  (cl-every
   (lambda (name)
     (eq (caledonia--alist-value name fields) 'Keep))
   '(summary start end_ location description categories recurrence alarms)))

(defun caledonia--protocol-alarm-attachment-request (attachment)
  "Convert structured response ATTACHMENT to its request variant."
  (let ((kind (caledonia--alist-value 'kind attachment))
        (value (caledonia--alist-value 'value attachment)))
    (pcase kind
      ('uri `(Uri ,value))
      ('binary `(Binary ,value))
      (_ (user-error "Unsupported alarm attachment kind: %S" kind)))))

(defun caledonia--protocol-alarm-other-request (property)
  "Convert structured response alarm PROPERTY to its request variant."
  (let ((kind (caledonia--alist-value 'kind property))
        (name (caledonia--alist-value 'name property))
        (value (caledonia--alist-value 'value property)))
    (pcase kind
      ('iana `(Iana ((name ,name)
                     (value ,value)
                     (parameters
                      ,(or (caledonia--alist-value 'parameters property)
                           '())))))
      ('x `(X ((namespace ,(caledonia--alist-value 'namespace property))
               (name ,name)
               (value ,value)
               (parameters ,(or (caledonia--alist-value 'parameters property)
                                '())))))
      (_ (user-error "Unsupported alarm extension kind: %S" kind)))))

(defun caledonia--protocol-alarm-request (alarm)
  "Convert structured response ALARM to the request representation."
  (let* ((action (caledonia--alist-value 'action alarm))
         (trigger (caledonia--alist-value 'trigger alarm))
         (trigger-kind (caledonia--alist-value 'kind trigger))
         (request-trigger
          (if (eq trigger-kind 'relative)
              `(Relative
                ((seconds ,(caledonia--alist-value 'seconds trigger))
                 (related ,(if (eq (caledonia--alist-value 'related trigger)
                                   'end)
                               'End
                             'Start))))
            `(Absolute ,(caledonia--alist-value 'value trigger))))
         (fields
          `((action ,(pcase action
                       ('audio 'Audio)
                       ('email 'Email)
                       ('none 'None_action)
                       (_ 'Display)))
            (trigger ,request-trigger))))
    (let ((parameters (caledonia--alist-value 'parameters trigger)))
      (when parameters
        (setq fields (append fields `((trigger_parameters ,parameters))))))
    (dolist (key '(repeat duration_seconds duration_parameters
                         repeat_parameters summary summary_parameters
                         description description_parameters attendee_values))
      (let ((entry (assq key alarm)))
        (when entry
          (setq fields (append fields (list entry))))))
    (let ((attachment (caledonia--alist-value 'attachment alarm)))
      (when attachment
        (setq fields
              (append fields
                      `((attachment
                         ,(caledonia--protocol-alarm-attachment-request
                           attachment))
                        (attachment_parameters
                         ,(or (caledonia--alist-value 'parameters attachment)
                              '())))))))
    (let ((other (caledonia--alist-value 'other alarm)))
      (when other
        (setq fields
              (append fields
                      `((other
                         ,(mapcar #'caledonia--protocol-alarm-other-request
                                  other)))))))
    fields))

(defun caledonia-event-form--parse-alarms (text)
  "Parse structured alarm list TEXT without evaluating it."
  (if (null text)
      nil
    (condition-case err
        (let* ((read-eval nil)
               (parsed (read-from-string text))
               (value (car parsed))
               (end (cdr parsed)))
          (unless (string-match-p "\\`[[:space:]]*\\'" (substring text end))
            (error "trailing data after structured value"))
          (unless (listp value)
            (error "alarms must be a list"))
          value)
      (error (user-error "Invalid structured alarms: %s"
                         (error-message-string err))))))

(defun caledonia-event-form--parse-string-list (text)
  "Parse TEXT as a list of category strings."
  (let ((value (caledonia-event-form--parse-alarms text)))
    (unless (cl-every #'stringp value)
      (user-error "Categories must be a list of strings"))
    value))

(defun caledonia--ics-rrule (source)
  "Extract the first RRULE value from iCalendar SOURCE."
  (when (and source
             (string-match "\\(?:\\`\\|\n\\)RRULE:\\([^\r\n]+\\)" source))
    (match-string 1 source)))

(defun caledonia-event-form-submit ()
  "Submit the event form."
  (interactive)
  (let* ((type caledonia-event-form--type)
         (calendar (caledonia-event-form--get-field "Calendar"))
         (summary (caledonia-event-form--get-field "Summary"))
         (start-str (caledonia-event-form--get-field "Start"))
         (end-str (caledonia-event-form--get-field "End"))
         (timezone (caledonia-event-form--get-field "Timezone"))
         (end-timezone (caledonia-event-form--get-field "End Timezone"))
         (recurrence (caledonia-event-form--get-field "Recurrence"))
         (clear-recurrence
          (caledonia-event-form--get-field "Clear Recurrence"))
         (alarms-str (caledonia-event-form--get-field "Alarms"))
         (categories-str (caledonia-event-form--get-field "Categories"))
         (location (caledonia-event-form--get-field "Location"))
         (description (caledonia-event-form--get-field "Description"))
         (return-buf caledonia-event-form--return-buffer))
    ;; Submit to server — let server validate, report errors via user-error
    (condition-case err
        (progn
          (pcase type
            ('create
             (unless (and calendar summary start-str)
               (user-error "Calendar, Summary, and Start are required"))
             (let ((fields
                    `((calendar ,calendar)
                      (summary ,summary)
                      (start ,(caledonia-event-form--time-input
                               start-str timezone)))))
               (when end-str
                 (setq fields
                       (append fields
                               `((end_
                                  ,(caledonia-event-form--end-input
                                    end-str end-timezone))))))
               (when location
                 (setq fields (append fields `((location ,location)))))
               (when description
                 (setq fields (append fields `((description ,description)))))
               (when recurrence
                 (setq fields
                       (append fields
                               `((recurrence ((rrule ,recurrence)))))))
               (when categories-str
                 (setq fields
                       (append fields
                               `((categories
                                  ,(caledonia-event-form--parse-string-list
                                    categories-str))))))
               (when alarms-str
                 (setq fields
                       (append fields
                               `((alarms
                                  ,(caledonia-event-form--parse-alarms
                                    alarms-str))))))
               (caledonia--send-request `(CreateEvent ,fields))
               (message "Event created: %s" summary)))
            ('edit
             (let ((fields
                    `((id ,caledonia-event-form--id)
                      (calendar_key ,caledonia-event-form--calendar-key)
                      (file ,caledonia-event-form--file)
                      (source_fingerprint
                       ,caledonia-event-form--source-fingerprint)
                      (summary ,(caledonia-event-form--patch
                                 "Summary" summary))
                      (start ,(caledonia-event-form--time-patch
                               "Start" "Timezone" start-str timezone))
                      (end_ ,(caledonia-event-form--end-patch
                              end-str end-timezone))
                      (location ,(caledonia-event-form--patch
                                  "Location" location))
                      (description ,(caledonia-event-form--patch
                                     "Description" description))
                      (categories ,(caledonia-event-form--patch
                                    "Categories" categories-str
                                    #'caledonia-event-form--parse-string-list))
                      (recurrence
                       ,(caledonia-event-form--recurrence-patch
                         recurrence clear-recurrence
                         caledonia-event-form--occurrence-start))
                      (alarms ,(caledonia-event-form--patch
                                "Alarms" alarms-str
                                #'caledonia-event-form--parse-alarms)))))
               (when caledonia-event-form--occurrence-start
                 (setq fields
                       (append
                        fields
                        `((occurrence_start
                           ,caledonia-event-form--occurrence-start)
                          (occurrence_timezone
                           ,caledonia-event-form--occurrence-timezone)))))
               (if (caledonia-event-form--all-patches-keep-p fields)
                   (message "Event unchanged: %s"
                            (or summary "(no summary)"))
                 (caledonia--send-request `(EditEvent ,fields))
                 (message "Event updated: %s"
                          (or summary "(no summary)"))))))
          ;; Only close form and refresh on success
          (quit-window t)
          (when (and return-buf (buffer-live-p return-buf))
            (switch-to-buffer return-buf)
            (when (eq major-mode 'caledonia-agenda-mode)
              (caledonia-refresh))))
      (error (user-error "%s" (error-message-string err))))))

(defun caledonia-event-form-cancel ()
  "Cancel the event form."
  (interactive)
  (let ((return-buf caledonia-event-form--return-buffer))
    (quit-window t)
    (when (and return-buf (buffer-live-p return-buf))
      (switch-to-buffer return-buf))
    (message "Cancelled.")))

(defun caledonia-add-event ()
  "Add a new event using a form buffer.
Fill in the fields, then press C-c C-c to create or C-c C-k to cancel.
Use C-c C-d on a date field to pick with org-read-date."
  (interactive)
  (let* ((calendars (caledonia--get-available-calendars))
         (calendar (if (= (length calendars) 1)
                       (car calendars)
                     (completing-read "Calendar: " calendars nil t)))
         (return-buf (current-buffer))
         (buf (get-buffer-create caledonia-event-form-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (caledonia-event-form-mode)
      (setq-local caledonia-event-form--type 'create)
      (setq-local caledonia-event-form--return-buffer return-buf)
      (let ((inhibit-read-only t))
        (caledonia-event-form--insert-field "Calendar" calendar)
        (caledonia-event-form--insert-field "Summary")
        (caledonia-event-form--insert-field "Start")
        (caledonia-event-form--insert-field "End")
        (caledonia-event-form--insert-field "Timezone")
        (caledonia-event-form--insert-field "End Timezone")
        (caledonia-event-form--insert-field "Recurrence")
        (caledonia-event-form--insert-field "Categories")
        (caledonia-event-form--insert-field "Alarms")
        (caledonia-event-form--insert-field "Location")
        (caledonia-event-form--insert-field "Description")
        (caledonia-event-form--insert-help))
      ;; Position cursor on Summary field value
      (goto-char (point-min))
      (re-search-forward "^Summary: " nil t))
    (switch-to-buffer-other-window buf)))

(defun caledonia--event-occurrence-context (event)
  "Return (START . TIMEZONE) only for an explicit occurrence EVENT."
  (when (caledonia--get-key 'is_occurrence event)
    (let ((start (caledonia--get-key 'occurrence_start event))
          (timezone (caledonia--get-key 'occurrence_timezone event)))
      (when (and start timezone) (cons start timezone)))))

(defun caledonia--event-form-source (event edit-occurrence)
  "Return occurrence EVENT or its canonical series master for the form."
  (if (or edit-occurrence
          (not (caledonia--event-occurrence-context event)))
      event
    (or (caledonia--get-key 'series_master event)
        (user-error "Server response is missing the recurrence master"))))

(defun caledonia-edit-event ()
  "Edit the event at point using a form buffer.
Fill in the fields, then press C-c C-c to save or C-c C-k to cancel.
Use C-c C-d on a date field to pick with org-read-date.
If the event is recurring, prompt whether to edit this occurrence or all."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (unless event
      (user-error "No event at point"))
    (let* ((occurrence-context (caledonia--event-occurrence-context event))
           (selected-summary (or (caledonia--get-key 'summary event) ""))
           (edit-occurrence
            (when occurrence-context
              (let ((scope (completing-read
                            (format "Edit '%s': " selected-summary)
                            '("This event" "All events in series")
                            nil t nil nil "This event")))
                (string= scope "This event"))))
           (event (caledonia--event-form-source event edit-occurrence))
           (id (caledonia--get-key 'id event))
           (calendar-key (caledonia--get-key 'calendar_key event))
           (file (caledonia--get-key 'file event))
           (source-fingerprint
            (caledonia--get-key 'source_fingerprint event))
           (summary (or (caledonia--get-key 'summary event) ""))
           (location (or (caledonia--get-key 'location event) ""))
           (description (or (caledonia--get-key 'description event) ""))
           (calendar (or (caledonia--get-key 'calendar event) ""))
           (start-value (caledonia--get-key 'start_value event))
           (end-value (caledonia--get-key 'end_value event))
           (end-time (caledonia--protocol-end-time end-value))
           (start-tz (or (caledonia--protocol-timezone-display start-value) ""))
           (end-tz (or (caledonia--protocol-timezone-display end-time) ""))
           (alarm-values (or (caledonia--get-key 'alarms_value event) '()))
           (category-values
            (or (caledonia--get-key 'categories_value event) '()))
           (categories
            (if category-values (prin1-to-string category-values) ""))
           (alarms
            (if alarm-values
                (prin1-to-string
                 (mapcar #'caledonia--protocol-alarm-request alarm-values))
              ""))
           (recurrence-value (caledonia--get-key 'recurrence_value event))
           (recurrence-set-value
            (or (caledonia--get-key 'recurrence_set_value event) '()))
           (recurrence-set-display
            (when recurrence-set-value
              (string-join recurrence-set-value " | ")))
           (recurrence
            (or (and recurrence-value
                     (caledonia--alist-value 'rrule recurrence-value))
                ""))
           (occurrence-start
            (when edit-occurrence (car occurrence-context)))
           (occurrence-timezone
            (when edit-occurrence (cdr occurrence-context)))
           (start-str (caledonia--protocol-time-display start-value))
           (end-str (caledonia--protocol-end-display end-value))
           (return-buf (current-buffer))
           (buf (get-buffer-create caledonia-event-form-buffer)))
      (with-current-buffer buf
        (erase-buffer)
        (caledonia-event-form-mode)
        (setq-local caledonia-event-form--type 'edit)
        (setq-local caledonia-event-form--id id)
        (setq-local caledonia-event-form--calendar-key calendar-key)
        (setq-local caledonia-event-form--file file)
        (setq-local caledonia-event-form--source-fingerprint
                    source-fingerprint)
        (setq-local caledonia-event-form--occurrence-start occurrence-start)
        (setq-local caledonia-event-form--occurrence-timezone
                    occurrence-timezone)
        (setq-local caledonia-event-form--return-buffer return-buf)
        (setq-local caledonia-event-form--original
                    `(("Summary" . ,(unless (string-empty-p summary) summary))
                      ("Start" . ,start-str)
                      ("End" . ,end-str)
                      ("Timezone" . ,(unless (string-empty-p start-tz) start-tz))
                      ("End Timezone" . ,(unless (string-empty-p end-tz) end-tz))
                      ("Recurrence" . ,(unless (string-empty-p recurrence) recurrence))
                      ("Alarms" . ,(unless (string-empty-p alarms) alarms))
                      ("Categories" . ,(unless (string-empty-p categories) categories))
                      ("Location" . ,(unless (string-empty-p location) location))
                      ("Description" . ,(unless (string-empty-p description) description))))
        (let ((inhibit-read-only t))
          (caledonia-event-form--insert-field "Calendar" calendar t)
          (caledonia-event-form--insert-field "Summary" summary)
          (caledonia-event-form--insert-field "Start" start-str)
          (caledonia-event-form--insert-field "End" end-str)
          (caledonia-event-form--insert-field "Timezone" start-tz)
          (caledonia-event-form--insert-field "End Timezone" end-tz)
          (unless occurrence-start
            (caledonia-event-form--insert-field "Recurrence Set"
                                                recurrence-set-display t)
            (caledonia-event-form--insert-field "Recurrence" recurrence)
            (when recurrence-set-value
              (caledonia-event-form--insert-field "Clear Recurrence")))
          (caledonia-event-form--insert-field "Categories" categories)
          (caledonia-event-form--insert-field "Alarms" alarms)
          (caledonia-event-form--insert-field "Location" location)
          (caledonia-event-form--insert-field "Description" description)
          (caledonia-event-form--insert-help))
        ;; Position cursor on Summary field value
        (goto-char (point-min))
        (re-search-forward "^Summary: " nil t))
      (switch-to-buffer-other-window buf))))

(defun caledonia-delete-event ()
  "Delete the event at point.
If the event is recurring, prompt whether to delete this occurrence or all."
  (interactive)
  (let ((event (get-text-property (point) 'event-data)))
    (unless event
      (user-error "No event at point"))
    (let* ((id (caledonia--get-key 'id event))
           (calendar-key (caledonia--get-key 'calendar_key event))
           (file (caledonia--get-key 'file event))
           (source-fingerprint
            (caledonia--get-key 'source_fingerprint event))
           (summary (or (caledonia--get-key 'summary event) "(no summary)"))
           (occurrence-context (caledonia--event-occurrence-context event))
           (scope (if occurrence-context
                      (completing-read
                       (format "Delete '%s': " summary)
                       '("This event" "All events in series")
                       nil t nil nil "This event")
                    "all")))
      (when (y-or-n-p (format "Delete %s? "
                               (if (string= scope "All events in series")
                                   (format "all events in series '%s'" summary)
                                 (format "event '%s'" summary))))
        (let ((fields `((id ,id)
                        (calendar_key ,calendar-key)
                        (file ,file)
                        (source_fingerprint ,source-fingerprint))))
          (when (and occurrence-context (string= scope "This event"))
            (setq fields
                  (append
                   fields
                   `((occurrence_start ,(car occurrence-context))
                     (occurrence_timezone ,(cdr occurrence-context))))))
          (caledonia--send-request `(DeleteEvent ,fields))
          (message "Event deleted: %s" summary)
          (when (eq major-mode 'caledonia-agenda-mode)
            (caledonia-refresh)))))))

;; Entry points

;;;###autoload
(defun caledonia-agenda (&optional from-date to-date)
  "Show an org-agenda style view of calendar events.
FROM-DATE and TO-DATE override defaults. With prefix arg, prompts for dates."
  (interactive
   (when current-prefix-arg
     (let ((dates (caledonia--read-date-range)))
       (list (car dates) (cdr dates)))))
  (let* ((from (or from-date caledonia-from-date))
         (to (or (and to-date (not (string-empty-p to-date)) to-date)
                 caledonia-to-date))
         (from-ymd (caledonia--resolve-date-to-ymd from))
         (to-ymd (caledonia--resolve-date-to-ymd to))
         (query `((to ,to)))
         (buffer (get-buffer-create caledonia--agenda-buffer)))
    (when (and from (not (string-empty-p from)))
      (setq query (append query `((from ,from)))))
    (let* ((payload (caledonia--send-request `(Query ,query)))
           (events (caledonia--get-events payload)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (caledonia-agenda-mode)
          (setq-local caledonia-agenda--query query)
          (setq-local caledonia-agenda--from-date from-ymd)
          (setq-local caledonia-agenda--to-date to-ymd)
          (caledonia--render-agenda events from-ymd to-ymd))
        (goto-char (point-min)))
      (pop-to-buffer-same-window buffer))))

;;;###autoload
(defun caledonia-search (text)
  "Search for TEXT in calendar events, showing results in agenda view."
  (interactive
   (list (read-string "Search for: " nil 'caledonia-text-history)))
  (let* ((from caledonia-from-date)
         (to caledonia-to-date)
         (from-ymd (caledonia--resolve-date-to-ymd from))
         (to-ymd (caledonia--resolve-date-to-ymd to))
         (query `((text ,text) (to ,to)))
         (buffer (get-buffer-create caledonia--agenda-buffer)))
    (when (and from (not (string-empty-p from)))
      (setq query (append query `((from ,from)))))
    (let* ((payload (caledonia--send-request `(Query ,query)))
           (events (caledonia--get-events payload)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (caledonia-agenda-mode)
          (setq-local caledonia-agenda--query query)
          (setq-local caledonia-agenda--from-date from-ymd)
          (setq-local caledonia-agenda--to-date to-ymd)
          (caledonia--render-agenda events from-ymd to-ymd))
        (goto-char (point-min)))
      (pop-to-buffer-same-window buffer))))

(provide 'caledonia)
;;; caledonia.el ends here
