;;; init-visuals.el --- Visual stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'evil)
;; (require 'hybrid-mode)
;; (require 'doom-modeline)
;; (require 'cl-macs)

(set-face-attribute 'doom-modeline-evil-normal-state nil :foreground "SeaGreen3")
(set-face-attribute 'doom-modeline-evil-replace-state nil :foreground "IndianRed2")
(set-face-attribute 'doom-modeline-evil-visual-state nil :foreground "LightCyan4")

(cl-flet
    ((dooms (evil-state)
            (let ((face (intern (concat "doom-modeline-evil-"
                                        (symbol-name evil-state)))))
                (face-attribute face :foreground nil t))))
  (setq-default
   evil-normal-state-cursor       `(,(dooms 'normal-state) box)
   evil-evilified-state-cursor    '("LightGoldenrod2"      box)
   evil-motion-state-cursor       '("MediumPurple1"        box)
   evil-visual-state-cursor       `(,(dooms 'visual-state) box)

   evil-hybrid-state-cursor       '("SteelBlue3"           box)
   evil-emacs-state-cursor        '("DodgerBlue2"          box)
   evil-insert-state-cursor       `(,(dooms 'insert-state) box)

   evil-replace-state-cursor      '("IndianRed2"           (hbar . 2))
   evil-lisp-state-cursor         '("HotPink2"             box)

   evil-iedit-state-cursor        '("IndianRed2"           box)
   evil-iedit-insert-state-cursor '("IndianRed2"           (bar . 2)))
  )

(provide 'init-visuals)
;;; init-visuals.el ends here
