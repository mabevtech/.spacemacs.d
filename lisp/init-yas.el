;;; init-yas.el --- YASnippet tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'yasnippet)
;; (require 'evil)
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

(provide 'init-yas)
;;; init-yas.el ends here
