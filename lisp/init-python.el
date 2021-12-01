;;; init-python.el --- Python tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'evil)
;; (require 'company)
;; (require 'python)
;; (require 'yasnippet)
;; (require 'core-keybindings)
;; (require 'flycheck)
(require 'init-prog)

;; https://github.com/syl20bnr/spacemacs/issues/10638#issuecomment-386519064
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; Send and output current line with C-RET

;; Function to evaluate AND OUTPUT current line or selection on Python mode
(defun python-shell-send-region-or-line-and-show-output
    (start end &optional msg)
  "Alternative to python-shell-send-region."
  (interactive
   (list (if mark-active (region-beginning) (line-beginning-position))
         (if mark-active (region-end) (line-end-position))
         t))
  (let* ((region (buffer-substring-no-properties start end))
         (process (python-shell-get-process-or-error msg)))
    (message "Sent: %s..." (string-trim region))
    (python-shell-send-string region process)))

;; Add function to major mode menu
(spacemacs/set-leader-keys-for-major-mode
  'python-mode "o s l" 'python-shell-send-region-or-line-and-show-output)

(evil-define-key (list 'insert 'hybrid 'normal) python-mode-map
  (kbd "<C-return>") 'python-shell-send-region-or-line-and-show-output)


(add-hook 'python-mode-hook
          (lambda ()
            ;; Override 'fixed value so regions work with snippets ($0)
            (with-eval-after-load 'yasnippet (setq yas-indent-line 'auto))
            (setq evil-shift-width 4)))

;; Restrict syntax checking to flake8 only
(setq-default flycheck-disabled-checkers '(python-mypy python-pylint))

(provide 'init-python)
;;; init-python.el ends here
