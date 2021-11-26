;;; init-emacs.el --- Emacs stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exec-path-from-shell)

;; Make sure that emacs will have $PATH variables from default shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Use regex searches by default
(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)

(setq garbage-collection-messages t)

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

;; Use hash of file name for auto save files
(defun my-shorten-auto-save-file-name (&rest args)
  "Override variable `buffer-file-name' with its SHA1."
  (let ((buffer-file-name
         (when buffer-file-name (sha1 buffer-file-name))))
    (apply args)))

(advice-add 'make-auto-save-file-name :around
            #'my-shorten-auto-save-file-name)

(provide 'init-emacs)
;;; init-emacs.el ends here
