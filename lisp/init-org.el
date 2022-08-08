;;; init-org.el --- Org stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'org)
;; (require 'org-id)
;; (require 'org-capture)
;; (require 'org-agenda)
;; (require 'helm-mode)

(defvar org-mode-map)

;;; org-capture

(defun mabo3n/org-capture-find-file-function (file)
  "Find FILE to insert the capture template.

This mimics the \"file+headline\" option's default behavior
to find the file, found in `org-capture-set-target-location'."
  (set-buffer (org-capture-target-buffer file))
  (unless (derived-mode-p 'org-mode)
    (org-display-warning
     (format "Capture requirement: switching buffer %S to Org mode"
             (current-buffer)))
    (org-mode))
  (org-capture-put-target-region-and-position)
  (widen))

(defun mabo3n/org-capture-find-headline-function (headline &optional level)
  "Find (or create) HEADLINE to insert the capture template.

This mimics the \"file+headline\" option's default behavior
to find the headline, found in `org-capture-set-target-location'.

The default behavior is to try to find a headline at any level,
and to create a top (1) level headline if not found.

This function also accepts an optional LEVEL argument which,
if is a number, restrict the search to and use LEVEL for creation."
  (goto-char (point-min))
  (if (and (re-search-forward (format org-complex-heading-regexp-format
                                      (regexp-quote headline))
                              nil t)
           (or (and (numberp level) (= (org-outline-level) level))
               t))
      (beginning-of-line)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (concat (make-string (or level 1) ?*) " ") headline "\n")
    (beginning-of-line 0)))

(defun mabo3n/org-capture-read-headline (&optional match prompt allow-new)
  "Read a headline name matching MATCH in current buffer.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
PROMPT is the text of the completion. Defaults to \"Headline: \".
ALLOW-NEW if non-nil lets the user input any string, even if it doesn't
match an existing headline.

When called interactively, prompts for MATCH string first.
With a \\[universal-argument], ALLOW-NEW is t."
  (interactive "sMatch string: \ni\nP")
  (let ((headlines
         (org-map-entries (apply-partially 'org-entry-get nil "ITEM")
                          match))
        (prompt (or prompt "Headline: ")))
    (helm-comp-read prompt headlines :must-match (not allow-new))))

(defun mabo3n/org-capture-dive-to-headline (match-sequence &optional allow-new)
  "Read a headline according to MATCH-SEQUENCE in current buffer.

This acts like a recursive version of `mabo3n/org-capture-read-headline',
where MATCH-SEQUENCE is a list of MATCH strings or (MATCH . PROMPT) entries.

For example:
  '(nil      ;; Prompt for any headline first
    (\"+country+tropics\" . \"Choose a country: \")
    (nil . \"Which state:\")
    \"+beach-cold\")

Note that headline search is performed by plain text.
If multiple headlines have the same text, the first one is chosen.
Point is left in the line of the last searched subheading.

ALLOW-NEW if non-nil lets the user input any string
as the last headline, even if it doesn't match an existing one.

When called interactively, prompts for MATCH-SEQUENCE string first.
With a \\[universal-argument], ALLOW-NEW is t."
  (interactive "xRaw match-sequence: \nP")
  (save-restriction
    (cl-loop
     with length = (length match-sequence)
     for entry in match-sequence
     for match = (if (consp entry) (car entry) entry)
     for prompt = (if (consp entry) (cdr entry) nil)
     for index from 1
     for lastp = (= index length)
     for headline = (mabo3n/org-capture-read-headline
                     match prompt (and lastp allow-new))
     do (mabo3n/org-capture-find-headline-function
         headline
         ;; FIXME if match-sequence has 1 entry and allow-new is
         ;; non-nil, "new" headings will be created with the level
         ;; of the entry under point when the function was called.
         (when lastp (1+ (org-outline-level))))
        (org-narrow-to-subtree))))

(defun mabo3n/org-capture-template-read-headline-link (file match-sequence)
  "Return a link to a headline read through MATCH-SEQUENCE in FILE.

This can be used in templates to expand to a link to a headline,
which is chosen interactively by a sequential of [parent] headline prompts
according to MATCH-SEQUENCE.
See function `mabo3n/org-capture-dive-to-headline' for more details."
  (with-current-buffer (org-capture-target-buffer file)
     (mabo3n/org-capture-dive-to-headline match-sequence)
     ;; apparently this doesn't work non-interactively
     ;; https://narkive.com/RtJ6Kjrt.1
     (call-interactively 'org-store-link))
  (with-temp-buffer
    (org-insert-last-stored-link 1)             ;; striping the \n
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

;;; org-capture / Tasks

(defconst mabo3n/default-tasks-file (expand-file-name "tasks" org-directory)
  "Default tasks file.")
(defconst mabo3n/default-tasks-headline "New"
  "Default tasks headline in tasks file.")
(defvar mabo3n/tasks-file mabo3n/default-tasks-file
  "File to store `org-capture' task entries.")
(defvar mabo3n/tasks-headline mabo3n/default-tasks-headline
  "Headline to store `org-capture' task entries.")

(defun mabo3n/org-capture-tasks-find-location-function (&optional prompt-defaults)
  "Captures into `mabo3n/tasks-file' within `mabo3n/tasks-headline'.

This provides an alternative for org-capture's file+headline target
with dynamic files and headlines. Code is adapted from file+headline
option code in `org-capture-set-target-location'.

If PROMPT-DEFAULTS is non-nil, prompts for the file with
`mabo3n/default-tasks-file' selected, and defaults to
`mabo3n/default-tasks-headline' as headline (which can be changed
inside the `org-capture' buffer with `org-capture-refile')."
  (let ((file (or (and prompt-defaults
                       (helm-read-file-name
                        "File: "
                        ;; :test (apply-partially #'string-suffix-p ".org")
                        :initial-input mabo3n/default-tasks-file))
                  mabo3n/tasks-file))
        (headline (or (and prompt-defaults mabo3n/default-tasks-headline)
                      mabo3n/tasks-headline)))
    (mabo3n/org-capture-find-file-function file)
    (mabo3n/org-capture-find-headline-function headline)))

;;; org-capture / Jobs

(defun mabo3n/org-capture-jobs-read-new-company ()
  "Read a (new) company name for the Jobs / Company template."
  (with-current-buffer (org-capture-target-buffer
                        "~/docs/jobs/companies.org")
    (mabo3n/org-capture-read-headline
     "LEVEL=2+company" "Company name: " t)))

(defun mabo3n/org-capture-jobs-position-find-headline-function ()
  "Find location function for Jobs / Position capture."
  (mabo3n/org-capture-dive-to-headline '(("LEVEL=2+company" . "Companies: ")))
  (save-restriction
    (org-narrow-to-subtree)
    (mabo3n/org-capture-find-headline-function "Positions" 3)
    (org-set-tags ":position:")))

;;; org-capture / templates

(setq org-capture-templates
      '(("t" "Task" entry
         (function mabo3n/org-capture-tasks-find-location-function)
         "* TODO %?
  SCHEDULED: %t
  %U
  %a"
         :clock-resume t :kill-buffer t)

        ("T" "Task (prompt)" entry
         (function (lambda ()
                     (mabo3n/org-capture-tasks-find-location-function t)))
         "* TODO %?
  SCHEDULED: %t
  %U
  %a"
         :clock-resume t :kill-buffer t)

        ("j" "Journal entry" entry
         (file+olp+datetree "journal")
         "* %U
  %?"
         :tree-type week :kill-buffer t)

        ("n" "Note" entry
         (file "")  ;; "" => `org-default-notes-file'
         "* %? :note:
  %U"
         :clock-resume t :kill-buffer t)

        ("J" "Jobs")

        ("Jc" "Company" entry
         (file+headline "~/docs/jobs/companies.org" "Companies")
         "* REVIEW %(mabo3n/org-capture-jobs-read-new-company
                    )%^{Source}p%^{Source_link}p
  :LOGBOOK:
  - State \"REVIEW\"     from              %U
  :END:"
         :immediate-finish t)

        ("Jp" "Position" entry
         (file+function "~/docs/jobs/companies.org"
                        mabo3n/org-capture-jobs-position-find-headline-function)
         "* %^{Title} (%^{Minimum yoe}+ yoe)
  :PROPERTIES:
  :Yoe: %\\2
  :END:
  [[%^{Link}][Link]]%?"
         :jump-to-captured t)

        ("Ja" "Application" entry
         (file+headline "~/docs/jobs/processes.org" "Processes")
         "* %(mabo3n/org-capture-template-read-headline-link
              \"~/docs/jobs/companies.org\"
              '((\"LEVEL=2+company\"))) / %(helm-read-string
                                            \"Title: \")%^{Webpage}p%?"
         :immediate-finish t :jump-to-captured t)

        ("Ji" "Interview" entry
         (file+function "~/docs/jobs/processes.org"
                        (lambda ()
                          (mabo3n/org-capture-dive-to-headline
                           '(("LEVEL=2+process" . "Application: ")))))
         "* Interview (%^{Type|Phone screen|Technical screen|Other}) :interview:
  %^T"
         :immediate-finish t :jump-to-captured t)

        ("Js" "Step" entry
         (file+function "~/docs/jobs/processes.org"
                        (lambda ()
                          (mabo3n/org-capture-dive-to-headline
                           '(("LEVEL=2+process" . "Application: ")))))
         "* %^{Step}
  DEADLINE: %^t"
         :immediate-finish t :jump-to-captured t)
        ))

;; Open org capture buffer in insert state (Adding to end of list
;; cause `spacemacs//org-capture-start' manually sets to normal state)
(add-hook 'org-capture-mode-hook 'evil-insert-state 1)
(add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

;;; org-agenda

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda #1="")
          (alltodo #1#)))))

(setq-default
 org-agenda-file-regexp
 (rx bos
     (or (seq (not (any ?.)) (0+ nonl) ".org")
         (seq (0+ nonl) (or "journal" "notes" "tasks")))
     eos))

;;; org-pomodoro

(with-eval-after-load 'org-pomodoro
  (setq-default org-pomodoro-time-format "%.2m")
  (if (package-installed-p 'all-the-icons)
      (setq-default org-pomodoro-format "%s"
                    org-pomodoro-short-break-format "%s"
                    org-pomodoro-long-break-format "%s")
    (setq-default org-pomodoro-format "P~%s"
                  org-pomodoro-short-break-format "B~%s"
                  org-pomodoro-long-break-format "L~%s")))

;;; org-cite

(defvar org-cite-global-bibliography)

(let ((bib (list (expand-file-name "docs/My Library.bib" mabo3n/home-dir))))
  (setq org-cite-global-bibliography bib
        ;; org-cite-insert-processor   'citar
        ;; org-cite-follow-processor   'citar
        ;; org-cite-activate-processor 'citar
        ;; citar-bibliography bib
        ))

;; (setq org-link-parameters
;;       (delq (assoc "cite" org-link-parameters) org-link-parameters))
;; (push (list "cite"
;;             :complete (lambda ()
;;                         (with-temp-buffer
;;                           (mabo3n/helm-bibtex-insert)
;;                           (buffer-substring-no-properties (point-min) (point-max)))))
;;       org-link-parameters)


;;; misc

;; Always show whitespace in org-mode
(defun mabo3n/show-trailing-whitespace ()
  "Show trailing whitespace."
  (setq show-trailing-whitespace t))
(add-hook 'org-mode-hook #'mabo3n/show-trailing-whitespace)

;; Bind `mabo3n/helm-bibtex-insert'
(with-eval-after-load 'init-helm-bibtex
  (define-key org-mode-map (kbd "C-c C-'") #'mabo3n/helm-bibtex-insert))

;; Make `org-insert-link' smart when inserting URLs
;; From https://xenodium.com/emacs-dwim-do-what-i-mean/

(defvar org-link-any-re)
(defvar org-mode-map)

(defun mabo3n/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences.

If an http URL is on top of kill ring, bypass the prompt for a link
and use the retrieved page title as default description.
If also the region is active, use that as description and replace
the region with the link, not prompting for anything.

Fallback to `org-insert-link' otherwise."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "Description: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

(define-key org-mode-map (kbd "C-c C-l") #'mabo3n/org-insert-link-dwim)

(setq-default
 org-cycle-separator-lines 2
 org-adapt-indentation t
 org-refile-targets '((nil :maxlevel . 1))
 org-refile-allow-creating-parent-nodes 'confirm
 org-log-into-drawer t
 org-id-link-to-org-use-id 'use-existing
 org-startup-folded 'show2levels
 org-hide-emphasis-markers t)

(setq-default
 org-default-notes-file (expand-file-name "notes" org-directory)
 org-archive-location ".archive.%s::"
 org-archive-file-header-format
 (concat ";;; -*- mode: org; -*-\n"
         "Archived entries from file =%s=:\n"
         "\n"))

(provide 'init-org)
;;; init-org.el ends here
