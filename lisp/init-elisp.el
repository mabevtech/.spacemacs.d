;;; init-elisp.el --- Emacs-lisp stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-prog)

;;; snippet expansion

(add-hook 'emacs-lisp-mode-hook
          'mabo3n/enable-snippet-expansion-on-completion)

(provide 'init-elisp)
;;; init-elisp.el ends here
