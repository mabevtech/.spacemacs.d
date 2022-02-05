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

(defvar mabo3n/default-inner-function-text-obj-function #'evil-inner-paragraph
  "Symbol for the default \"inner function\" text object.")
(defvar mabo3n/default-a-function-text-obj-function     #'evil-a-paragraph
  "Symbol for the default \"a function\" text object.")

(defun mabo3n/bind-function-text-objects-for-current-language ()
  "Bind \"if\" and \"af\" to function text objects of current language.

Try to bind to \"mabo3n/evil-OBJTYPE-LANGUAGE-function\" where OBJTYPE
is \"inner\" or \"a\", and LANGUAGE is the current mode's programming
language.

If symbol's function definition is void, bind to variables
`mabo3n/default-inner-function-text-obj-function' and
`mabo3n/default-a-function-text-obj-function' instead.

For instance, if current buffer's `major-mode' is `python-mode',
\"if\" will make the current evil operator operate over
the text object defined by function `mabo3n/evil-inner-python-function'
if it is defined, or `mabo3n/default-inner-function-text-obj-function'
if it is not."
  (let* ((current-mode     (symbol-name major-mode))
         (current-language (string-trim-right current-mode "-mode"))
         (inner-text-obj-symbol
          (intern (string-join
                   (list "mabo3n/evil-inner-" current-language "-function"))))
         (a-text-obj-symbol
          (intern (string-join
                   (list "mabo3n/evil-a-"     current-language "-function")))))
    (unless (fboundp inner-text-obj-symbol)
      (setq inner-text-obj-symbol mabo3n/default-inner-function-text-obj-function))
    (unless (fboundp a-text-obj-symbol)
      (setq a-text-obj-symbol mabo3n/default-a-function-text-obj-function))
    ;; (message "Binding function text objects:\n  if -> %s\n  af -> %s"
    ;;          inner-text-obj-symbol
    ;;          a-text-obj-symbol)
    (dolist (map `(,evil-operator-state-local-map
                   ,evil-visual-state-local-map))
       (define-key map (kbd "if") inner-text-obj-symbol)
       (define-key map (kbd "af") a-text-obj-symbol))))

(add-hook 'prog-mode-hook
          #'mabo3n/bind-function-text-objects-for-current-language)

(provide 'init-prog)
;;; init-prog.el ends here
