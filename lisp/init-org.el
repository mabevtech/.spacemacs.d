;;; init-org.el --- Org stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'org)
;; (require 'org-capture)
;; (require 'org-agenda)
;; (require 'helm-mode)

;;; org-capture

(defconst mabo3n/default-tasks-file (expand-file-name "tasks.org" org-directory)
  "Default tasks file.")
(defconst mabo3n/default-tasks-headline "New"
  "Default tasks headline in tasks file.")
(defvar mabo3n/tasks-file mabo3n/default-tasks-file
  "File to store `org-capture' task entries.")
(defvar mabo3n/tasks-headline mabo3n/default-tasks-headline
  "Headline to store `org-capture' task entries.")

(defun mabo3n/org-capture-file+headline-function (&optional prompt-defaults)
  "Captures into `mabo3n/tasks-file' within `mabo3n/tasks-headline'.

This provides an alternative for org-capture's file+headline target
with dynamic files and headlines. Code is adapted from file+headline
option code in `org-capture-set-target-location'.

If PROMPT-DEFAULTS is non-nil, prompts for the file with
`mabo3n/default-tasks-file' selected, and defaults to
`mabo3n/default-tasks-headline' as headline (which can be changed
inside the `org-capture' buffer with `org-capture-refile')."
  (let ((buffer
         (org-capture-target-buffer
          (or (and prompt-defaults (helm-read-file-name
                                    "File: "
                                    ;; :test (apply-partially #'string-suffix-p ".org")
                                    :initial-input mabo3n/default-tasks-file))
              mabo3n/tasks-file)))
        (headline (or (and prompt-defaults mabo3n/default-tasks-headline)
                      mabo3n/tasks-headline)))
    (set-buffer buffer)
	  (unless (derived-mode-p 'org-mode)
	    (org-display-warning
	     (format "Capture requirement: switching buffer %S to Org mode"
		           (current-buffer)))
	    (org-mode))
	  (org-capture-put-target-region-and-position)
	  (widen)
	  (goto-char (point-min))
    (if (re-search-forward (format org-complex-heading-regexp-format
					                         (regexp-quote headline))
				                   nil t)
	      (beginning-of-line)
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (insert "* " headline "\n")
	    (beginning-of-line 0))))

(setq org-capture-templates
      '(("t" "Task" entry
         (function mabo3n/org-capture-file+headline-function)
         "* TODO %?\n  %U\n  %a" :clock-resume t)

        ("T" "Task (prompt)" entry
         (function (lambda () (mabo3n/org-capture-file+headline-function t)))
         "* TODO %?\n  %U\n  %a" :clock-resume t)

        ("j" "Journal entry" entry
         (file+olp+datetree "journal.org")
         "* %U\n  %?" :empty-lines 1)

        ("n" "Note" entry (file "")  ;; "" => `org-default-notes-file'
         "* %? :NOTE:\n%U" :clock-resume t)
        ))

;; Open org capture buffer in insert state (Adding to end of list
;; cause `spacemacs//org-capture-start' manually sets to normal state)
(add-hook 'org-capture-mode-hook 'evil-insert-state 1)

;;; misc

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda #1="")
          (alltodo #1#)))))

(add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))

(provide 'init-org)
;;; init-org.el ends here
