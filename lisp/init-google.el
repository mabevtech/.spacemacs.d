;;; init-google.el --- Better usage of google suggest/translate -*- lexical-binding: t -*-

;;; Commentary:

;; Make google suggest/translate follow Spacemacs conventions better (IMO).

;;; Code:

;; (require 'helm)
;; (require 'helm-net)
;; (require 'thingatpt)
;; (require 'google-translate-default-ui)
;; (require 'core-keybindings)

;; Workaround to fix google-translate-at-point
;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-470756933
(defun google-translate--search-tkk ()
  "Search TKK."
  (list 430675 2721866130))

(defun mabo3n/helm-google-suggest (&optional input)
  (interactive)
  (let ((input (or (and (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end)))
                   input))
        ;; This prevents some character repetition while typing
        (helm-input-idle-delay 0.4))
    (helm :sources 'helm-source-google-suggest
          :buffer "*helm google*"
          :input input
          :delayed t)))

(defun mabo3n/helm-google-suggest-at-point ()
  (interactive)
  (let ((input (word-at-point t)))
    (unless input (message "Nothing at point."))
    (mabo3n/helm-google-suggest input)))

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

(setq google-translate-pop-up-buffer-set-focus t)

(provide 'init-google)
;;; init-google.el ends here
