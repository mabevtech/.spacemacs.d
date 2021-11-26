;;; init-evil.el --- Evil tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'evil)
;; (require 'evil-cleverparens-text-objects)
;; ;; (require 'hybrid-mode)

;; Use a box cursor on insert/hybrid modes instead of bar one
(setq-default evil-insert-state-cursor
              (list (car evil-insert-state-cursor) 'box)
              evil-hybrid-state-cursor
              (list (car evil-hybrid-state-cursor) 'box))

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

(provide 'init-evil)
;;; init-evil.el ends here
