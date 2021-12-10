;;; init-csharp.el --- CSharp tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'evil)
;; (require 'helm)
;; (require 'projectile)
;; (require 'omnisharp)
;; (require 'core-keybindings)
(require 'init-prog)
(require 'init-c)

;;; projectile

(defun mabo3n/projectile-dotnet-solution-p ()
  "Whether the current project is a dotnet solution."
  (projectile-verify-file-wildcard "?*.sln"))

(with-eval-after-load 'projectile
  (projectile-register-project-type
   'dotnet-sln #'mabo3n/projectile-dotnet-solution-p
   :src-dir "src/"
   :test-dir "tests/"
   :compile "cd src/ && dotnet build"
   :run "cd src/ && dotnet run"
   :test "cd tests/ && dotnet test"
   :test-suffix "Tests"))

;;; omnisharp

(defun mabo3n/switch-to-omnisharp-log-buffer ()
  "Switch to omnisharp log buffer."
  (interactive)
  (switch-to-buffer "*omnisharp-log*"))

(defun mabo3n/switch-to-omnisharp-log-buffer-other-window ()
  "Opens omnisharp log buffer in another window."
  (interactive)
  (switch-to-buffer-other-window "*omnisharp-log*"))

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode
  "s l" #'mabo3n/switch-to-omnisharp-log-buffer
  "s L" #'mabo3n/switch-to-omnisharp-log-buffer-other-window)

(setq-default omnisharp-debug nil
              omnisharp-imenu-support t)

;;; snippet expansion

(add-hook 'csharp-mode-hook
          'mabo3n/prioritize-snippet-expansion-on-completion)

;;; evil text objects

;; (spacemacs|define-text-object-regexp "m" "method" "" "")

(let ((method-header-regexp
       (concat "^\\(\\(?2:[[:space:]]*\\)[[:alpha:]].*\\)\\(\n"
               "\\2[[:space:]]*[^[:space:]\n"
               "].*\\)\\{0,3\\}\\(\n"
               "[[:space:]]+\\)?\\(?5:{[[:space:]]*$\\|=>\\)")))
  (evil-define-text-object evil-inner-csharp-function (count &optional beg end type)
    (save-excursion
      (let* ((p (point))
             (match-data
              (and (re-search-backward method-header-regexp nil t)
                   (match-data))))
        ;; TODO what if there's no class/namespace and match-data is nil?

        ;; `re-search-backward' only matches pattern ending BEFORE `point.'.
        ;; If we're under a function header, the PREVIOUS function (or class)
        ;; header will be matched instead. So we try to match the next one
        ;; and, if it starts before previous point, that's the one we want.
        (unless (and (goto-char (point-at-eol)) ; avoid the same pattern
                     (re-search-forward method-header-regexp nil t)
                     (<= (match-beginning 0) p))
          (goto-char p)
          (set-match-data match-data))
        (when-let ((beg (match-beginning 0))
                   (end (match-end 0))
                   (indent-string (match-string-no-properties 2))
                   (end-string (match-string-no-properties 5)))
          (list beg
                (if (string= end-string "=>")
                    (and (search-forward ";" nil t) (1+ (point-at-eol)))
                  (and (re-search-forward (concat "^" indent-string "}") nil t)
                       (1+ (point-at-eol)))))
          ))))
  (define-key evil-inner-text-objects-map "f" 'evil-inner-csharp-function))

;;; misc

(defun mabo3n/csharp-set-shift-width ()
  (interactive)
  (setq-default evil-shift-width c-basic-offset))

(add-hook 'csharp-mode-hook
          'mabo3n/csharp-set-shift-width)

(provide 'init-csharp)
;;; init-csharp.el ends here
