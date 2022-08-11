;;; init-helm-bibtex.el --- helm-bibtex config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'helm)

(defvar helm-bibtex-full-frame)
(defvar bibtex-completion-bibliography)

(with-eval-after-load 'helm-bibtex
  (setq helm-bibtex-full-frame nil)
  (setq bibtex-completion-bibliography
        (concat "/mnt/c/Users/marcel.bornancin/OneDrive - Anheuser-Busch InBev/"
                "My Documents/My Library.bib")))

;; Make `helm-bibtex' insert citations with org-cite syntax (in org files)

(defvar bibtex-completion-format-citation-functions)

(with-eval-after-load 'helm-bibtex
 (setq bibtex-completion-format-citation-functions
       (delq (assoc 'org-mode bibtex-completion-format-citation-functions)
             bibtex-completion-format-citation-functions))
 (push (cons 'org-mode 'bibtex-completion-format-citation-org-cite)
       bibtex-completion-format-citation-functions))

;; Define wrapper for `helm-bibtex' to easily insert citations

;; (declare-function helm-build-sync-source "helm-source")
;; (declare-function helm-make-actions "helm-lib")
(declare-function helm-bibtex "helm-bibtex")

(defvar mabo3n/helm-source-bibtex-insert
  (helm-build-sync-source "BibTeX entries"
    :header-name (lambda (name)
                   (format "%s%s: " name (if helm-bibtex-local-bib " (local)" "")))
    :candidates 'helm-bibtex-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    ;; TODO Cycle between showing the entry and killing the buffer
    :persistent-action 'helm-bibtex-show-entry
    :action (helm-make-actions
             "Insert citation"            'helm-bibtex-insert-citation
             "Insert reference"           'helm-bibtex-insert-reference
             "Insert BibTeX key"          'helm-bibtex-insert-key
             "Insert BibTeX entry"        'helm-bibtex-insert-bibtex
             "Open PDF, URL or DOI"       'helm-bibtex-open-any
             "Open URL or DOI in browser" 'helm-bibtex-open-url-or-doi
             "Edit notes"                 'helm-bibtex-edit-notes
             "Attach PDF to email"        'helm-bibtex-add-PDF-attachment
             ;; "Show entry"                 'helm-bibtex-show-entry
             "Add PDF to library"         'helm-bibtex-add-pdf-to-library))
  "Source for searching in BibTeX files. Prioritizes insert-related actions.")

(defun mabo3n/helm-bibtex-insert (&optional arg local-bib input)
  "Wrap `helm-bibtex' using `mabo3n/helm-source-bibtex-insert'.

Forward ARG, LOCAL-BIB and INPUT to wrapped function."
  (interactive "P")
  (require 'helm-bibtex)
  (let ((helm-source-bibtex mabo3n/helm-source-bibtex-insert))
    (helm-bibtex arg local-bib input)))

(provide 'init-helm-bibtex)
;;; init-helm-bibtex.el ends here
