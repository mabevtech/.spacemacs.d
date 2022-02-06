;;; init-elisp.el --- Emacs-lisp stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-prog)
(require 'evil-cleverparens-text-objects)

;;; snippet expansion

(add-hook 'emacs-lisp-mode-hook
          'mabo3n/prioritize-snippet-expansion-on-completion)

;;; function text objects

;; Make a/inner resemble word's (text object).
;; "inner" for lines spanning defun,
;; "a" for lines spanning defun + surrounding blank lines

(evil-define-text-object mabo3n/evil-inner-emacs-lisp-function
  (count &optional beg end type)
  "Like `evil-cp-a-defun' (yes, \"a\" defun) but line-wise."
  (if (evil-cp--inside-form-p)
      (let ((bounds (evil-cp--top-level-bounds)))
        (evil-range (car bounds) (1+ (cdr bounds)) 'line :expanded t))
    (error "Not inside a sexp")))

(evil-define-text-object mabo3n/evil-a-emacs-lisp-function
  (count &optional beg end type)
  "Like `evil-cp-a-defun' but include surrounding blank lines.

Select the top-level form line-wise, and include succeeding blank lines.
If none is found, include preceding blank lines (if existent)."
  (save-excursion
    (if (evil-cp--inside-form-p)
        (let* ((bounds             (evil-cp--top-level-bounds))
               (end-of-succeeding-blank-lines
                (and (goto-char (cdr bounds))
                     (re-search-forward (rx (1+ bol (0+ space) ?\n)) nil t)))
               (beg
                (or (and end-of-succeeding-blank-lines (car bounds))
                    (progn
                      (goto-char (car bounds))
                      (re-search-backward (rx (or bos (not (any space ?\n)))) nil t)
                      (or (and (string= "" (match-string-no-properties 0))
                               ;; we're at the beginning of the buffer
                               (car bounds))
                          (and (forward-line 1)
                               (point))))))
               (end (or end-of-succeeding-blank-lines (cdr bounds))))
          (evil-range beg end 'line :expanded t))
     (error "Not inside a sexp"))))

(provide 'init-elisp)
;;; init-elisp.el ends here
