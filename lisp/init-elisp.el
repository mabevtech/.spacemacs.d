;;; init-elisp.el --- Emacs-lisp stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-prog)

;;; snippet expansion

(add-hook 'emacs-lisp-mode-hook
          'mabo3n/prioritize-snippet-expansion-on-completion)

;;; function text objects

(defalias #'mabo3n/evil-inner-emacs-lisp-function #'evil-cp-inner-defun)
(defalias #'mabo3n/evil-a-emacs-lisp-function     #'evil-cp-a-defun)

(provide 'init-elisp)
;;; init-elisp.el ends here
