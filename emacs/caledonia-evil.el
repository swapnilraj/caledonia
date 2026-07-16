;;; caledonia-evil.el --- Evil bindings for Caledonia -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ryan Gibb
;;
;; Author: Ryan Gibb <ryan@freumh.org>
;; Maintainer: Ryan Gibb <ryan@freumh.org>
;; Version: 0.5.0
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1") (evil))
;; URL: https://ryan.freumh.org/caledonia.html
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides Evil bindings for Caledonia.
;;
;;; Code:

(require 'caledonia)

(declare-function evil-define-key* "evil" (state keymap &rest bindings))
(declare-function evil-make-overriding-map "evil" (map state))
(declare-function evil-normalize-keymaps "evil" ())

;; Only load evil integration if evil is available
(require 'evil nil t)

(defun caledonia-evil--setup-bindings ()
  "Set up Evil keybindings for `caledonia-agenda-mode`."
  (when (featurep 'evil)
    (evil-define-key* 'normal caledonia-agenda-mode-map
      (kbd "RET") 'caledonia-show-event
      (kbd "M-RET") 'caledonia-open-event-file
      "r" 'caledonia-refresh
      "a" 'caledonia-add-event
      "e" 'caledonia-edit-event
      "d" 'caledonia-delete-event
      "s" 'caledonia-search
      "q" 'quit-window
      "?" 'caledonia-agenda-help)))

(defun caledonia-evil--setup-integration ()
  "Set up Evil integration for Caledonia agenda mode."
  (when (and (featurep 'evil) (bound-and-true-p evil-mode))
    (evil-make-overriding-map caledonia-agenda-mode-map 'normal)
    (evil-normalize-keymaps)
    (caledonia-evil--setup-bindings)))

(add-hook 'caledonia-agenda-mode-hook #'caledonia-evil--setup-integration)

(defun caledonia-evil--setup-form-bindings ()
  "Set up Evil keybindings for `caledonia-event-form-mode`."
  (when (featurep 'evil)
    (evil-define-key* 'normal caledonia-event-form-mode-map
      "ZZ" 'caledonia-event-form-submit
      "ZQ" 'caledonia-event-form-cancel
      (kbd "C-c C-c") 'caledonia-event-form-submit
      (kbd "C-c C-k") 'caledonia-event-form-cancel
      (kbd "C-c C-d") 'caledonia-event-form-pick-date)))

(add-hook 'caledonia-event-form-mode-hook #'caledonia-evil--setup-form-bindings)

(provide 'caledonia-evil)
;;; caledonia-evil.el ends here
