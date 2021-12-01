;;; init-prog.el --- Programming related stuff -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; (require 'cl-lib)
;; (require 'yasnippet)

(defun mabo3n/enable-snippet-expansion-on-completion ()
  (cl-pushnew 'yas-expand completion-at-point-functions))

(provide 'init-prog)
;;; init-prog.el ends here
