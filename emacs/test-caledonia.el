;;; test-caledonia.el --- ERT tests for Caledonia -*- lexical-binding: t; -*-

(require 'ert)
(require 'caledonia)

(defvar caledonia-test-reader-evaluated nil)

(ert-deftest caledonia-transport-correlates-interleaved-responses ()
  (let* ((buffer (generate-new-buffer " *caledonia-test-process*"))
         (process (start-process "caledonia-test-cat" buffer "cat")))
    (unwind-protect
        (progn
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (clrhash caledonia--pending-responses)
          (puthash "first" :pending caledonia--pending-responses)
          (puthash "second" :pending caledonia--pending-responses)
          (puthash "unquoted-id" :pending caledonia--pending-responses)
          (caledonia--server-filter
           process
           "(Response ((version 1) (request_id \"second\") (response (Ok Emp")
          (should (eq (gethash "second" caledonia--pending-responses) :pending))
          (caledonia--server-filter
           process
           "ty))))\n(Response ((version 1) (request_id \"first\") (response (Ok Empty))))\n")
          (should (equal (caledonia--alist-value
                          'request_id
                          (cadr (gethash "first" caledonia--pending-responses)))
                         "first"))
          (should (equal (caledonia--alist-value
                          'request_id
                          (cadr (gethash "second" caledonia--pending-responses)))
                         "second"))
          (caledonia--server-filter
           process
           "(Response ((version 1) (request_id unquoted-id) (response (Ok Empty))))\n")
          (should (gethash "unquoted-id" caledonia--pending-responses)))
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (kill-buffer buffer))))

(ert-deftest caledonia-transport-normalizes-server-shaped-text-atoms ()
  (let* ((buffer (generate-new-buffer " *caledonia-normalize-test*"))
         (process (start-process "caledonia-normalize-cat" buffer "cat")))
    (unwind-protect
        (progn
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (clrhash caledonia--pending-responses)
          (puthash "normalize" :pending caledonia--pending-responses)
          (puthash "calendars" :pending caledonia--pending-responses)
          (caledonia--server-filter
           process
           (concat
            "(Response ((version 1) (request_id normalize) "
            "(response (Ok (Events (((id plain-id) (calendar_key personal) "
            "(categories_value (work project-x)) "
            "(start_value ((kind tzid) (value 2026-07-15T09:30:45) "
            "(tzid Europe/London))) "
            "(end_value ((kind dtend) (value ((kind floating) "
            "(value 2026-07-15T10:30:45))))) "
            "(start_tz Europe/London) (end_tz FLOATING) "
            "(occurrence_timezone Europe/London))))))))\n"
            "(Response ((version 1) (request_id calendars) "
            "(response (Ok (Calendars (personal team))))))\n"))
          (let* ((response (gethash "normalize"
                                    caledonia--pending-responses))
                 (status (caledonia--alist-value 'response (cadr response)))
                 (payload (cadr status))
                 (event (car (cadr payload)))
                 (start (caledonia--alist-value 'start_value event))
                 (end (caledonia--alist-value 'end_value event))
                 (end-time (caledonia--alist-value 'value end))
                 (calendars-response
                  (gethash "calendars" caledonia--pending-responses))
                 (calendars-status
                  (caledonia--alist-value 'response
                                           (cadr calendars-response))))
            ;; Envelope and protocol discriminator atoms remain symbols.
            (should (eq (car response) 'Response))
            (should (eq (car status) 'Ok))
            (should (eq (car payload) 'Events))
            (should (eq (caledonia--alist-value 'kind start) 'tzid))
            (should (eq (caledonia--alist-value 'kind end) 'dtend))
            (should (eq (caledonia--alist-value 'kind end-time) 'floating))
            ;; Sexplib's unquoted textual atoms are normalized for form code.
            (should (equal (caledonia--alist-value 'calendar_key event)
                           "personal"))
            (should (equal (caledonia--alist-value 'categories_value event)
                           '("work" "project-x")))
            (should (equal (caledonia--alist-value 'value start)
                           "2026-07-15T09:30:45"))
            (should (equal (caledonia--alist-value 'tzid start)
                           "Europe/London"))
            (should (equal (caledonia--alist-value 'value end-time)
                           "2026-07-15T10:30:45"))
            (should (equal (caledonia--alist-value 'start_tz event)
                           "Europe/London"))
            (should (equal (caledonia--alist-value 'end_tz event)
                           "FLOATING"))
            (should (equal (caledonia--alist-value
                            'occurrence_timezone event)
                           "Europe/London"))
            (should (equal (cadr (cadr calendars-status))
                           '("personal" "team")))))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest caledonia-transport-allows-multiframe-chunk-over-one-mebibyte ()
  (let* ((buffer (generate-new-buffer " *caledonia-multiframe-test*"))
         (process (start-process "caledonia-multiframe-cat" buffer "cat"))
         (caledonia-server-frame-limit 1048576)
         (padding (make-string 525000 ?\s))
         (first
          (concat
           "(Response ((version 1) (request_id large-first) "
           "(response (Ok Empty))))"
           padding))
         (second
          (concat
           "(Response ((version 1) (request_id large-second) "
           "(response (Ok Empty))))"
           padding))
         (chunk (concat first "\n" second "\n")))
    (unwind-protect
        (progn
          (should (> (length chunk) caledonia-server-frame-limit))
          (should (<= (length first) caledonia-server-frame-limit))
          (should (<= (length second) caledonia-server-frame-limit))
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (clrhash caledonia--pending-responses)
          (puthash "large-first" :pending caledonia--pending-responses)
          (puthash "large-second" :pending caledonia--pending-responses)
          (caledonia--server-filter process chunk)
          (should (process-live-p process))
          (should (listp (gethash "large-first"
                                  caledonia--pending-responses)))
          (should (listp (gethash "large-second"
                                  caledonia--pending-responses)))
          (should (string-empty-p caledonia--server-line-buffer)))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest caledonia-transport-rejects-unsolicited-and-oversized-frames ()
  (let* ((buffer (generate-new-buffer " *caledonia-bounds-test*"))
         (process (start-process "caledonia-bounds-cat" buffer "cat"))
         (caledonia-server-frame-limit 128))
    (unwind-protect
        (progn
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (clrhash caledonia--pending-responses)
          (caledonia--server-filter
           process
           "(Response ((version 1) (request_id unknown) (response (Ok Empty))))\n")
          (should-not (gethash "unknown" caledonia--pending-responses))
          (setq caledonia-server-frame-limit 64)
          (caledonia--server-filter process (make-string 65 ?x))
          (should (string-empty-p caledonia--server-line-buffer))
          (should-not (process-live-p process)))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest caledonia-server-stderr-remains-bounded ()
  (let ((error-buffer
         (get-buffer-create caledonia--server-error-buffer-name))
        (caledonia-server-log-limit 32))
    (unwind-protect
        (progn
          (with-current-buffer error-buffer (erase-buffer))
          (caledonia--server-error-filter nil (make-string 80 ?x))
          (with-current-buffer error-buffer
            (should (= (buffer-size) caledonia-server-log-limit))
            (should (equal (buffer-string) (make-string 32 ?x)))))
      (when (buffer-live-p error-buffer) (kill-buffer error-buffer)))))

(ert-deftest caledonia-transport-rejects-oversized-complete-frame ()
  (let* ((buffer (generate-new-buffer " *caledonia-complete-bound-test*"))
         (process (start-process "caledonia-complete-bound-cat" buffer "cat"))
         (caledonia-server-frame-limit 64))
    (unwind-protect
        (progn
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (caledonia--server-filter process (concat (make-string 65 ?x) "\n"))
          (should (string-empty-p caledonia--server-line-buffer))
          (should-not (process-live-p process)))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest caledonia-transport-rejects-trailing-reader-data ()
  (let* ((buffer (generate-new-buffer " *caledonia-trailing-test*"))
         (process (start-process "caledonia-trailing-cat" buffer "cat"))
         (error-buffer
          (get-buffer-create caledonia--server-error-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer error-buffer (erase-buffer))
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (clrhash caledonia--pending-responses)
          (puthash "trailing" :pending caledonia--pending-responses)
          (caledonia--server-filter
           process
           (concat
            "(Response ((version 1) (request_id trailing) "
            "(response (Ok Empty)))) trailing-garbage\n"))
          (should (eq (gethash "trailing" caledonia--pending-responses)
                      :pending))
          (with-current-buffer error-buffer
            (should (string-match-p "trailing data after response frame"
                                    (buffer-string)))))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest caledonia-transport-sentinel-resets-state ()
  (let ((process (start-process
                  "caledonia-test-sentinel" (generate-new-buffer " *caledonia-test*")
                  "cat")))
    (unwind-protect
        (progn
          (setq caledonia--server-process process
                caledonia--server-line-buffer "partial"
                caledonia--handshake-complete t)
          (puthash "pending" :pending caledonia--pending-responses)
          (caledonia--server-sentinel process "finished")
          (should-not caledonia--server-process)
          (should-not caledonia--handshake-complete)
          (should (string-empty-p caledonia--server-line-buffer))
          (should (= (hash-table-count caledonia--pending-responses) 0)))
      (when (process-live-p process) (delete-process process))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(ert-deftest caledonia-stale-process-cannot-corrupt-replacement-session ()
  (let* ((old-buffer (generate-new-buffer " *caledonia-old-process*"))
         (new-buffer (generate-new-buffer " *caledonia-new-process*"))
         (old-process (start-process "caledonia-old-cat" old-buffer "cat"))
         (new-process (start-process "caledonia-new-cat" new-buffer "cat")))
    (unwind-protect
        (progn
          (setq caledonia--server-process new-process
                caledonia--server-line-buffer "new-partial"
                caledonia--handshake-complete t)
          (clrhash caledonia--pending-responses)
          (puthash "new-pending" :pending caledonia--pending-responses)
          (caledonia--server-filter
           old-process
           "(Response ((version 1) (request_id old) (response (Ok Empty))))\n")
          (caledonia--server-sentinel old-process "finished")
          (should (eq caledonia--server-process new-process))
          (should caledonia--handshake-complete)
          (should (equal caledonia--server-line-buffer "new-partial"))
          (should (eq (gethash "new-pending" caledonia--pending-responses)
                      :pending))
          (should-not (gethash "old" caledonia--pending-responses)))
      (dolist (process (list old-process new-process))
        (set-process-sentinel process #'ignore)
        (when (process-live-p process) (delete-process process)))
      (setq caledonia--server-process nil)
      (dolist (buffer (list old-buffer new-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest caledonia-protocol-reader-disables-reader-evaluation ()
  (let* ((buffer (generate-new-buffer " *caledonia-reader-test*"))
         (process (start-process "caledonia-reader-cat" buffer "cat"))
         (caledonia-test-reader-evaluated nil))
    (unwind-protect
        (progn
          (setq caledonia--server-process process
                caledonia--server-line-buffer "")
          (clrhash caledonia--pending-responses)
          (caledonia--server-filter
           process
           "(Response ((version 1) (request_id malicious) (response #.(setq caledonia-test-reader-evaluated t))))\n")
          (should-not caledonia-test-reader-evaluated)
          (should-not (gethash "malicious" caledonia--pending-responses))
          (should-error
           (caledonia-event-form--parse-alarms
            "(#.(setq caledonia-test-reader-evaluated t))")
           :type 'user-error)
          (should-not caledonia-test-reader-evaluated))
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (when (eq caledonia--server-process process)
        (setq caledonia--server-process nil))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest caledonia-form-structured-values-reject-trailing-data ()
  (should-error
   (caledonia-event-form--parse-alarms
    "(((action Display) (trigger (Relative ((seconds -60) (related Start)))))) trailing")
   :type 'user-error)
  (should-error
   (caledonia-event-form--parse-string-list "(\"work\") trailing")
   :type 'user-error)
  (should
   (equal (caledonia-event-form--parse-string-list
           "(\"work\" \"home\")  \n\t")
          '("work" "home"))))

(ert-deftest caledonia-edit-calendar-field-is-read-only-identity ()
  (with-temp-buffer
    (caledonia-event-form-mode)
    (caledonia-event-form--insert-field "Calendar" "personal" t)
    (goto-char (point-min))
    (search-forward "personal")
    (let ((position (1- (point))))
      (should (get-text-property position 'read-only))
      (goto-char position)
      (should-error (insert "x") :type 'text-read-only))))

(ert-deftest caledonia-form-calendar-time-preserves-kind-and-seconds ()
  (should
   (equal (caledonia-event-form--time-input
           "2026-10-25 01:30:45" "Europe/London")
          '((kind (Tzid "Europe/London"))
            (value "2026-10-25T01:30:45"))))
  (should
   (equal (caledonia-event-form--time-input "2026-07-15" nil)
          '((kind Date) (value "2026-07-15"))))
  (should
   (equal (caledonia-event-form--time-input
           "2026-07-15 12:00:01" "FLOATING")
          '((kind Floating) (value "2026-07-15T12:00:01")))))

(ert-deftest caledonia-form-patch-distinguishes-keep-clear-set ()
  (let ((caledonia-event-form--original
         '(("Location" . "Room 1") ("Description" . nil))))
    (should (eq (caledonia-event-form--patch "Location" "Room 1") 'Keep))
    (should (eq (caledonia-event-form--patch "Location" nil) 'Clear))
    (should
     (equal (caledonia-event-form--patch "Description" "Notes")
            '(Set "Notes")))))

(ert-deftest caledonia-form-structured-patches-preserve-semantics ()
  (let ((caledonia-event-form--original
         '(("Categories" . "(\"work\" \"important\")")
           ("Recurrence" . "FREQ=WEEKLY;BYDAY=MO")
           ("Alarms" . "(((action Display) (trigger (Relative ((seconds -60) (related Start))))))"))))
    (should
     (equal (caledonia-event-form--patch
             "Categories" "(\"work\" \"personal\")"
             #'caledonia-event-form--parse-string-list)
            '(Set ("work" "personal"))))
    (should
     (equal (pcase (caledonia-event-form--patch
                    "Recurrence" "FREQ=DAILY")
              (`(Set ,rrule) `(Set ((rrule ,rrule)))))
            '(Set ((rrule "FREQ=DAILY")))))
    (should (eq (caledonia-event-form--patch "Alarms" nil) 'Clear))))

(ert-deftest caledonia-form-rdate-only-series-can-be-cleared-explicitly ()
  (let ((caledonia-event-form--original '(("Recurrence" . nil))))
    (should
     (eq (caledonia-event-form--recurrence-patch nil nil nil) 'Keep))
    (should
     (eq (caledonia-event-form--recurrence-patch nil "yes" nil) 'Clear))
    (should
     (equal
      (caledonia-event-form--recurrence-patch "FREQ=DAILY" nil nil)
      '(Set ((rrule "FREQ=DAILY")))))
    (should
     (eq (caledonia-event-form--recurrence-patch nil "yes" t) 'Keep))
    (should-error
     (caledonia-event-form--recurrence-patch nil "maybe" nil)
     :type 'user-error)))

(ert-deftest caledonia-form-unchanged-all-day-end-is-keep ()
  (let ((caledonia-event-form--original
         '(("End" . "2026-07-16") ("End Timezone" . nil))))
    (should
     (eq (caledonia-event-form--end-patch "2026-07-16" nil) 'Keep))))

(ert-deftest caledonia-form-duration-end-supports-keep-clear-set ()
  (let ((caledonia-event-form--original
         '(("End" . "duration:3723") ("End Timezone" . nil))))
    (should
     (equal (caledonia--protocol-end-display
             '((kind duration) (seconds 3723)))
            "duration:3723"))
    (should (eq (caledonia-event-form--end-patch "duration:3723" nil) 'Keep))
    (should (eq (caledonia-event-form--end-patch nil nil) 'Clear))
    (should
     (equal (caledonia-event-form--end-patch "duration:7200" nil)
            '(Set (Duration_seconds 7200))))))

(ert-deftest caledonia-occurrence-scope-requires-typed-query-identity ()
  (should
   (equal
    (caledonia--event-occurrence-context
     '((recurring true)
       (is_occurrence true)
       (occurrence_start "2026-07-15T08:00:00Z")
       (occurrence_timezone "Europe/London")))
    '("2026-07-15T08:00:00Z" . "Europe/London")))
  (should-not
   (caledonia--event-occurrence-context
    '((recurring true) (start_utc "2026-07-15T09:00:00Z")))))

(ert-deftest caledonia-all-series-form-uses-canonical-master-payload ()
  (let* ((master
          '((id "series-id")
            (calendar_key "work")
            (file "/calendars/work/series-id.ics")
            (source_fingerprint "sha")
            (start_value ((kind floating) (value "2026-07-01T09:00:00")))
            (recurrence_value ((rrule "FREQ=WEEKLY;COUNT=4")))))
         (occurrence
          `((is_occurrence true)
            (occurrence_start "2026-07-15T08:00:00Z")
            (occurrence_timezone "Europe/London")
            (start_value ((kind floating) (value "2026-07-15T09:00:00")))
            (series_master ,master))))
    (should (eq (caledonia--event-form-source occurrence t) occurrence))
    (should (eq (caledonia--event-form-source occurrence nil) master))
    (dolist (field '(id calendar_key file source_fingerprint))
      (should (caledonia--get-key field
                                  (caledonia--event-form-source occurrence nil))))
    (should
     (equal
      (caledonia--alist-value
       'rrule
       (caledonia--get-key
        'recurrence_value
        (caledonia--event-form-source occurrence nil)))
      "FREQ=WEEKLY;COUNT=4"))))

(ert-deftest caledonia-unchanged-whitespace-sensitive-edit-form-does-not-send ()
  (with-temp-buffer
    (caledonia-event-form-mode)
    (setq-local caledonia-event-form--type 'edit)
    (setq-local caledonia-event-form--id "event-id")
    (setq-local caledonia-event-form--calendar-key "personal")
    (setq-local caledonia-event-form--file "/calendar/event.ics")
    (setq-local caledonia-event-form--source-fingerprint "opaque-token")
    (setq-local caledonia-event-form--original
                '(("Summary" . "  Unchanged  ")
                  ("Start" . "2026-07-15 10:00:45")
                  ("End" . "2026-07-15 11:00:45")
                  ("Timezone" . "UTC")
                  ("End Timezone" . "UTC")
                  ("Recurrence" . "FREQ=WEEKLY;COUNT=2")
                  ("Categories" . "(\"work\")")
                  ("Alarms" . "(((action Display) (trigger (Relative ((seconds -60) (related Start))))))")
                  ("Location" . " Room ")
                  ("Description" . "\n  Notes  \n")))
    (dolist (entry '( ("Calendar" . "Personal")
                      ("Summary" . "  Unchanged  ")
                      ("Start" . "2026-07-15 10:00:45")
                      ("End" . "2026-07-15 11:00:45")
                      ("Timezone" . "UTC")
                      ("End Timezone" . "UTC")
                      ("Recurrence" . "FREQ=WEEKLY;COUNT=2")
                      ("Categories" . "(\"work\")")
                      ("Alarms" . "(((action Display) (trigger (Relative ((seconds -60) (related Start))))))")
                      ("Location" . " Room ")
                      ("Description" . "\n  Notes  \n")))
      (caledonia-event-form--insert-field (car entry) (cdr entry)))
    (let ((sent nil))
      (cl-letf (((symbol-function 'caledonia--send-request)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'quit-window) #'ignore)
                ((symbol-function 'message) #'ignore))
        (caledonia-event-form-submit))
      (should-not sent))))

(ert-deftest caledonia-structured-alarm-round-trips-to-request-shape ()
  (should
   (equal
    (caledonia--protocol-alarm-request
     '((action display)
       (trigger ((kind relative) (seconds -901) (related end)))
       (repeat 2)
       (duration_seconds 30)
       (description "Wake up")
       (attachment ((kind binary) (value "AAEC/w==")))
       (other (((kind iana) (name "ACKNOWLEDGED")
                (value "20260715T100000Z") (parameters ()))
               ((kind x) (namespace "TRACE") (name "ID")
                (value "alarm-42")
                (parameters (((name "X-LABEL") (value "\"secure\"")))))))))
    '((action Display)
      (trigger (Relative ((seconds -901) (related End))))
      (repeat 2)
      (duration_seconds 30)
      (description "Wake up")
      (attachment (Binary "AAEC/w=="))
      (attachment_parameters ())
      (other ((Iana ((name "ACKNOWLEDGED")
                     (value "20260715T100000Z")
                     (parameters ())))
              (X ((namespace "TRACE") (name "ID")
                  (value "alarm-42")
                  (parameters (((name "X-LABEL")
                                (value "\"secure\""))))))))))))

(ert-deftest caledonia-empty-agenda-renders-requested-dates ()
  (with-temp-buffer
    (caledonia--render-agenda nil '(2026 7 15) '(2026 7 16))
    (let ((contents (buffer-string)))
      (should (string-match-p "15 July 2026" contents))
      (should (string-match-p "16 July 2026" contents)))))

(ert-deftest caledonia-agenda-uses-query-timezone-local-fields ()
  (with-temp-buffer
    (caledonia--render-agenda
     '(((start "2026-07-15T18:30:00Z")
        (end "2026-07-15T19:30:00Z")
        (start_local "2026-07-15T19:30:00")
        (end_local "2026-07-15T20:30:00")
        (summary "London evening")
        (calendar "Personal")
        (is_date nil)))
     '(2026 7 15) '(2026 7 15))
    (let ((contents (buffer-string)))
      (should (string-match-p "19:30-20:30" contents))
      (should-not (string-match-p "18:30-19:30" contents)))))

(ert-deftest caledonia-timezone-discovery-includes-top-level-and-nested-zones ()
  (let ((root (make-temp-file "caledonia-zones-" t))
        (caledonia--timezone-list nil))
    (unwind-protect
        (progn
          (dolist (name '("UTC" "America/Argentina/Buenos_Aires"
                          "right/UTC" "zone.tab"))
            (let ((file (expand-file-name name root)))
              (make-directory (file-name-directory file) t)
              (write-region "zone" nil file nil 'silent)))
          (cl-letf (((symbol-function 'cl-find-if)
                     (lambda (_predicate _directories) root)))
            (should
             (equal (caledonia--timezone-list)
                    '("America/Argentina/Buenos_Aires" "UTC")))))
      (delete-directory root t))))

(provide 'test-caledonia)
;;; test-caledonia.el ends here
