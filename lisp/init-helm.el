;;; init-helm.el --- Helm stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'core-keybindings)
(require 'subr-x)
(require 'helm)
(require 'dash)
(require 'evil-jumps)

(with-eval-after-load 'helm-regexp
  (spacemacs/set-leader-keys "x r h" #'helm-regexp))

(with-eval-after-load 'evil-jumps
  (defvar mabo3n/point-before-helm-jump-in-buffer)

  (defun mabo3n/helm-jump-in-buffer ()
    "Store `point' value then prompt."
    (interactive)
    (setq mabo3n/point-before-helm-jump-in-buffer (point))
    (lazy-helm/spacemacs/helm-jump-in-buffer))

  (spacemacs/set-leader-keys "s j" #'mabo3n/helm-jump-in-buffer)

  (defun mabo3n/set-evil-jump-previous-position ()
    "If `point' has changed, set an evil jump to its previous position."
    (when-let* ((prev-point mabo3n/point-before-helm-jump-in-buffer)
                (changedp (not (eq (point) prev-point))))
      (evil-set-jump prev-point)))

  (add-hook 'imenu-after-jump-hook #'mabo3n/set-evil-jump-previous-position))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-SPC") 'helm-company))

(with-eval-after-load 'helm-company
  ;; Make documentation lookup work in 'helm-company
  (defun mabo3n/helm-company-display-document-buffer (buffer)
    "Display buffer without predefined actions (nil)."
    (with-current-buffer buffer
      (goto-char (point-min)))
    (display-buffer buffer nil))
  (advice-add #'helm-company-display-document-buffer :override
              #'mabo3n/helm-company-display-document-buffer))

;; Make `helm-find-files' always expand symlinks to directories
;; This was the default behavior but now requires a prefix arg
;; https://github.com/emacs-helm/helm/issues/1121
(defun mabo3n/helm-ff-always-expand-symlink-dirs (fun &rest args)
  "Set `current-prefix-arg' if current selection is a dir symlink.

Advice :around FUN with ARGS."
  (let ((candidate (car args)))
    (when (and candidate
               (not current-prefix-arg)
               (file-directory-p candidate)
               (file-symlink-p candidate))
      (message "Auto applying C-u to expand symlink")
      (setq current-prefix-arg '(4)))
    (apply fun args)))

(advice-add #'helm-find-files-persistent-action-if :around
            #'mabo3n/helm-ff-always-expand-symlink-dirs)

;; Define shortcuts for parameters in helm-ag queries
;; e.g. "Users -Grepo" -> "Users --iglob=**/*repo*/**"

(defconst mabo3n/helm-ag-query-options-transformations-alist
  '(("^-G\\(\\S-+\\)" . "--iglob=**/*\\1*/**"))
  "Helm-ag query options transformations.

Each entry has form (REGEXP . REPLACEMENT).")

(defun mabo3n/helm-ag-transform-query-options (&rest args)
  "Advice to transform options of `helm-ag--parse-options-and-query' (ARGS).

Map each (parsed) option with all matched transformations defined in
`mabo3n/helm-ag-query-options-transformations-alist'."
  (-let* (((options . query) (apply args))
          (case-fold-search nil)
          (options
           (--map
            (let ((transformed
                   (reduce
                    (lambda (opt transformation)
                      (replace-regexp-in-string (car transformation)
                                                (cdr transformation)
                                                opt
                                                nil))
                    mabo3n/helm-ag-query-options-transformations-alist
                    :initial-value it)))
              (message "%s -> %s" it transformed)
              transformed)
            options)))
    (cons options query)))

(advice-add #'helm-ag--parse-options-and-query :around
            #'mabo3n/helm-ag-transform-query-options)

(provide 'init-helm)
;;; init-helm.el ends here
