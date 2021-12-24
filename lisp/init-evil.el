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

;; TODO: customize evil cursor colors and modeline evil faces
;; https://stackoverflow.com/a/31747535
(macrolet
    ((doom-color (evil-state)
                 (let ((face (intern (concat "doom-modeline-evil-"
                                             (symbol-name evil-state)))))
                   (face-attribute face :foreground nil t))))
  (setq-default
   evil-normal-state-cursor       '((doom-color normal-state) box)
   evil-evilified-state-cursor    '("MediumPurple4" box)
   evil-visual-state-cursor       '("LightGoldenrod3" (hbar . 2))

   evil-hybrid-state-cursor       '("DodgerBlue"    box)
   evil-emacs-state-cursor        '("DodgerBlue2"   box)
   evil-insert-state-cursor       '("SteelBlue1"    box)

   evil-replace-state-cursor      '("chocolate"     (hbar . 2))
   evil-lisp-state-cursor         '("HotPink2"      box)

   evil-iedit-state-cursor        '("IndianRed2"    box)
   evil-iedit-insert-state-cursor '("IndianRed2"   (bar . 2))
   evil-operator-state-cursor     '(evil-half-cursor)))

;; doom-modeline-evil-insert-state

;; doom-modeline-evil-visual-state
;; doom-modeline-evil-normal-state
;; doom-modeline-evil-motion-state

;; doom-modeline-evil-replace-state

;; doom-modeline-evil-operator-state


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

(provide 'init-evil)
;;; init-evil.el ends here
