;;; init-prog.el --- Programming related stuff -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; (require 'cl-lib)
;; (require 'yasnippet)
;; (require 'evil)

;;; snippets

(defun mabo3n/prioritize-snippet-expansion-on-completion ()
  (interactive)
  (setq completion-at-point-functions
        (cons 'yas-expand
              (remove 'yas-expand completion-at-point-functions))))

;;; function text objects

(defvar mabo3n/default-a-function-text-obj-function 'evil-inner-paragraph
  "Symbol for the default \"a function\" text object.")
(defvar mabo3n/default-a-FUNCTION-text-obj-function 'evil-a-paragraph
  "Symbol for the default \"a FUNCTION\" text object.")

(defun mabo3n/bind-function-text-objects-for-current-language ()
  "Bind \"af\" and \"aF\" to function text objects of current language.

Try to bind to \"mabo3n/evil-a-<lang>-function\" and
\"mabo3n/evil-a-<lang>-FUNCTION\" where <lang> is the current mode's
programming language.

If the symbol's function definition is void, bind to variables
`mabo3n/default-a-function-text-obj-function' and
`mabo3n/default-a-FUNCTION-text-obj-function' respectively."
  (let* ((current-mode     (symbol-name major-mode))
         (current-language (string-trim-right current-mode "-mode"))
         (text-object-symbol
          (intern (string-join
                   (list "mabo3n/evil-a-" current-language "-function"))))
         (text-OBJECT-symbol
          (intern (string-join
                   (list "mabo3n/evil-a-" current-language "-FUNCTION")))))
    (unless (fboundp text-object-symbol)
      (setq text-object-symbol mabo3n/default-a-function-text-obj-function))
    (unless (fboundp text-OBJECT-symbol)
      (setq text-OBJECT-symbol mabo3n/default-a-FUNCTION-text-obj-function))
    (dolist (map `(,evil-operator-state-local-map
                   ,evil-visual-state-local-map))
       (define-key map (kbd "af") text-object-symbol)
       (define-key map (kbd "aF") text-OBJECT-symbol))))

(provide 'init-prog)
;;; init-prog.el ends here
