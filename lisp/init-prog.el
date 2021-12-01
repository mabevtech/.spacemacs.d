;;; init-prog.el --- Programming related stuff -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; (require 'cl-lib)
;; (require 'yasnippet)

(defun mabo3n/prioritize-snippet-expansion-on-completion ()
  (interactive)
  (setq completion-at-point-functions
        (cons 'yas-expand
              (remove 'yas-expand completion-at-point-functions))))

(provide 'init-prog)
;;; init-prog.el ends here
