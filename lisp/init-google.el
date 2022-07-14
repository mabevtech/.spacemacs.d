;;; init-google.el --- Config for google related features -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'thingatpt)
;; (require 'core-keybindings)

(spacemacs/set-leader-keys
  "s G" (key-binding (kbd "SPC s g")) ;; bind +grep search to capital g
  "s g" nil                           ;; unbind +grep stuff from small g
  "s g g" #'mabo3n/helm-google-suggest
  "s g G" #'mabo3n/helm-google-suggest-at-point
  "s g t" (key-binding (kbd "SPC x g"))
  "s g t t" #'mabo3n/google-translate-query-translate
  "s g t T" #'google-translate-at-point
  "s g t v" #'mabo3n/google-translate-query-translate-reverse
  "s g t V" #'google-translate-at-point-reverse)

(spacemacs/declare-prefix "s G" "grep")
(spacemacs/declare-prefix "s g" "Google")
(spacemacs/declare-prefix "s g t" "translate")

;;; google suggest

(with-eval-after-load 'helm
 (defun mabo3n/helm-google-suggest (&optional input)
   (interactive)
   (let ((input (or (and (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end)))
                    input))
         ;; This prevents some character repetition while typing
         (helm-input-idle-delay 0.2))
     (require 'helm-net)
     (helm :sources 'helm-source-google-suggest
           :buffer "*helm google*"
           :input input
           :delayed t)))

 (defun mabo3n/helm-google-suggest-at-point ()
   (interactive)
   (let ((input (word-at-point t)))
     (unless input (message "Nothing at point."))
     (mabo3n/helm-google-suggest input))))

;;; google translate

;; Workaround to fix google-translate-at-point
;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-470756933
(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))

(defun mabo3n/google-translate-query-translate (&rest args)
  (interactive)
  (if (use-region-p)
      (apply #'google-translate-at-point args)
    (apply #'google-translate-query-translate args)))

(defun mabo3n/google-translate-query-translate-reverse (&rest args)
  (interactive)
  (if (use-region-p)
      (apply #'google-translate-at-point-reverse args)
    (apply #'google-translate-query-translate-reverse args)))

(with-eval-after-load 'google-translate-core-ui
  (setq-default google-translate-pop-up-buffer-set-focus t))

(provide 'init-google)
;;; init-google.el ends here
