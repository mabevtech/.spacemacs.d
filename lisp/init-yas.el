;;; init-yas.el --- YASnippet tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'yasnippet)
;; (require 'evil)
;; (require 'cl)
;; (require 'helm)
;; (require 'subr-x)
;; ;; (require 'hybrid-mode)

;;; helm

(defun mabo3n/helm-yas ()
  "Wrapper of `spacemacs/helm-yas'.

Workaround to region on visual state mode not working with yasnippets
\(See URL
`https://github.com/emacs-evil/evil/issues/254#issuecomment-309839802')."
  (interactive)
  (if (not (evil-visual-state-p))
      (spacemacs/helm-yas)
    (let ((visual-line-selection-p (eq evil-visual-selection 'line))
          (point-on-region-end-p (> (point) (mark))))
      (setq yas-wrap-around-region ?y
            evil-this-register ?y)
      (call-interactively #'evil-substitute)
      (evil-set-register ?y (if visual-line-selection-p
                                (string-trim-right (evil-get-register ?y) "\n")
                              (evil-get-register ?y)))
      (let* ((beg (point))
             (snippet-chosen-p (spacemacs/helm-yas)))
        (unless snippet-chosen-p
          (evil-paste-from-register ?y)
          (evil-normal-state)
          (evil-visual-state)
          (goto-char beg)
          (when point-on-region-end-p (exchange-point-and-mark)))
        snippet-chosen-p))))

(define-key evil-insert-state-map (kbd "C-;") #'mabo3n/helm-yas)
(define-key evil-normal-state-map (kbd "C-;") #'mabo3n/helm-yas)
(define-key evil-visual-state-map (kbd "C-;") #'mabo3n/helm-yas)
(define-key evil-hybrid-state-map (kbd "C-;") #'mabo3n/helm-yas)

;;; misc

;; Fix for org src block expansion
;; https://www.reddit.com/r/emacs/comments/nj08dz/issues_with_yasnippet_in_emacs_272_lisp_error/
(with-eval-after-load 'init-org
  (defun mabo3n/org-mode-prepare-for-snippet-expansion ()
    "Set variable `org-src-tabs-act-natively' to nil."
    (setq mabo3n/org-src-tab-acts-natively org-src-tab-acts-natively)
    (setq org-src-tabs-act-natively nil))
  (defun mabo3n/org-mode-finalize-after-snippet-exit ()
    "Restore value of variable `org-src-tabs-act-natively'."
    (setq org-src-tabs-act-natively mabo3n/org-src-tab-acts-natively)
    (makunbound 'mabo3n/org-src-tab-acts-natively))

  (add-hook 'yas-before-expand-snippet-hook
            #'mabo3n/org-mode-prepare-for-snippet-expansion)
  (add-hook 'yas-after-exit-snippet-hook
            #'mabo3n/org-mode-finalize-after-snippet-exit))

;; Custom function to enable reusing some snippet in another
;; From the package author: https://stackoverflow.com/a/10366711
(defun yas/insert-by-name (name)
  (flet ((dummy-prompt
          (prompt choices &optional display-fn)
          (declare (ignore prompt))
          (or (find name choices :key display-fn :test #'string=)
              (throw 'notfound nil))))
    (let ((yas/prompt-functions '(dummy-prompt)))
      (catch 'notfound
        (yas/insert-snippet t)))))

(provide 'init-yas)
;;; init-yas.el ends here
