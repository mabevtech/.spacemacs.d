;;; init-csharp.el --- CSharp tweakings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yasnippet)
(require 'evil)
(require 'helm)
(require 'projectile)
(require 'omnisharp)
(require 'core-keybindings)

;;; projectile

(defun projectile-dotnet-solution-p ()
  "Whether the current project is a dotnet solution."
  (projectile-verify-file-wildcard "?*.sln"))

(with-eval-after-load 'projectile
  (projectile-register-project-type
   'dotnet-sln #'projectile-dotnet-solution-p
   :src-dir "src/"
   :test-dir "tests/"
   :compile "cd src/ && dotnet build"
   :run "cd src/ && dotnet run"
   :test "cd tests/ && dotnet test"
   :test-suffix "Tests"))

;;; omnisharp

(defun switch-to-omnisharp-log-buffer ()
  "Switch to omnisharp log buffer."
  (interactive)
  (switch-to-buffer "*omnisharp-log*"))

(defun switch-to-omnisharp-log-buffer-other-window ()
  "Opens omnisharp log buffer in another window."
  (interactive)
  (switch-to-buffer-other-window "*omnisharp-log*"))

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode
  "s l" #'switch-to-omnisharp-log-buffer
  "s L" #'switch-to-omnisharp-log-buffer-other-window)

(setq omnisharp-imenu-support t)
(setq-default omnisharp-debug t
              omnisharp-imenu-support t)

;;; misc

(add-hook 'csharp-mode-hook
          (lambda ()
            (setq evil-shift-width c-basic-offset)))

(provide 'init-csharp)
;;; init-csharp.el ends here
