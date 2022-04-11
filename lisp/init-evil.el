;;; init-evil.el --- Evil tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'evil)
;; (require 'evil-cleverparens-text-objects)
;; ;; (require 'hybrid-mode)

(define-key minibuffer-local-map (kbd "C-<escape>") #'evil-normal-state)

(defun mabo3n/evil-yank-visual-state (beg end &rest args)
  "Yank then restore point (i.e. <y> <gv> <escape>).

Forward BEG, END and ARGS to `evil-yank'"
  (interactive "r")
  (apply #'evil-yank beg end args)
  (evil-visual-restore)
  (evil-exit-visual-state))

(define-key evil-visual-state-map (kbd "y") #'mabo3n/evil-yank-visual-state)
(define-key evil-insert-state-map (kbd "C-y") #'yank)

;; Prevent evil from overriding some edebug keys (e.g. c, i, n)
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(define-key evil-inner-text-objects-map "f" #'evil-cp-inner-defun)
(define-key evil-outer-text-objects-map "f" #'evil-cp-a-defun)

;; (evil-make-overriding-map Info-mode-map nil)
;; (add-hook 'Info-mode-hook #'evil-normalize-keymaps)

(defun mabo3n/goto-last-edit () (interactive) (evil-goto-mark ?. t))
(define-key evil-normal-state-map (kbd "U") #'mabo3n/goto-last-edit)

;; Try to be less aggressive with the undos
(setq-default evil-want-fine-undo t)

(provide 'init-evil)
;;; init-evil.el ends here
