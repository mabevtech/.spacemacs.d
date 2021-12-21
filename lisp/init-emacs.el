;;; init-emacs.el --- Emacs stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'exec-path-from-shell)
;; (require 'edebug)

;; Make sure that emacs will have $PATH variables from default shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Use regex searches by default
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)
(global-set-key (kbd "M-%") #'query-replace-regexp)
(global-set-key (kbd "C-M-%") #'query-replace)

;; I constantly press this by mistake
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x C-c C-c") #'save-buffers-kill-terminal)

(setq garbage-collection-messages t)

(setq edebug-print-length nil)

(defun mabo3n/enable-auto-reload-dir-locals-vars-on-save ()
  "From: https://emacs.stackexchange.com/a/13096."

  (defun mabo3n/reload-dir-locals-for-current-buffer ()
    "Reload dir-locals for the current buffer."
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))

  (defun mabo3n/reload-dir-locals-for-all-buffer-in-this-directory ()
    "Reload dir-locals for every buffer sharing `default-directory' with current's."
    (interactive)
    (let ((dir default-directory))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (equal default-directory dir)
            (mabo3n/reload-dir-locals-for-current-buffer))))))

  (add-hook 'emacs-lisp-mode-hook
            (defun enable-autoreload-for-dir-locals ()
              (when (and (buffer-file-name)
                         (equal dir-locals-file
                                (file-name-nondirectory (buffer-file-name))))
                (add-hook 'after-save-hook
                          #'mabo3n/reload-dir-locals-for-all-buffer-in-this-directory
                          nil t)))))

(mabo3n/enable-auto-reload-dir-locals-vars-on-save)

;; Always allow marking a dir-local variable permanently as safe.
;; https://emacs.stackexchange.com/a/10989
(defun mabo3n/risky-local-variable-p (sym &optional _ignored)
  "Non-nil if SYM could be dangerous as a file-local variable.

It is dangerous IFF its `risky-local-variable'property is non-nil.
This is the same as `risky-local-variable-p' but without the 2nd condition."
  (condition-case nil
      (setq sym (indirect-variable sym))
    (error nil))
  (get sym 'risky-local-variable))

(advice-add 'risky-local-variable-p :override
            #'mabo3n/risky-local-variable-p)

;; Use hash of file name for auto save files
(defun my-shorten-auto-save-file-name (&rest args)
  "Override variable `buffer-file-name' with its SHA1."
  (let ((buffer-file-name
         (when buffer-file-name (sha1 buffer-file-name))))
    (apply args)))

(advice-add 'make-auto-save-file-name :around
            #'my-shorten-auto-save-file-name)

;; Make query replace not stop when encountering read-only text
;; This is useful while replacing on helm-ag-edit buffer
(setq query-replace-skip-read-only t)

(with-eval-after-load 're-builder
  (defun mabo3n/reb-copy ()
    "Copy as `reb-copy' but respect RE string syntax.

This allows pasting the regexp on `query-replace-regexp' completion
just fine. See URL `https://emacs.stackexchange.com/a/51190'."
    (interactive)
    (if (not (eq reb-re-syntax 'string))
        (reb-copy)
      (with-current-buffer reb-buffer
        (reb-update-regexp)
        (goto-char (point-min))
        (re-search-forward "\"")
        (let* ((beg (point))
               (regexp
                (progn
                  (goto-char (point-max))
                  (re-search-backward "\"")
                  (buffer-substring-no-properties beg (point)))))
          (kill-new regexp)
          (message "Copied regexp `%s' to kill-ring" regexp)))))

  (define-key reb-mode-map (kbd "C-c y") #'mabo3n/reb-copy))

(provide 'init-emacs)
;;; init-emacs.el ends here
