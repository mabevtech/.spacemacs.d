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

(defun mabo3n/projectile-dotnet-project-p ()
  "Whether the current project is a dotnet one."
  (projectile-verify-file-wildcard "?*.csproj"))

;; NOTE: These won't be detected automatically as we're using
;; a predicate function instead of static-named marker files
(with-eval-after-load 'projectile
  (projectile-register-project-type
   'dotnet-sln
   #'mabo3n/projectile-dotnet-solution-p
   :src-dir "src/"
   :test-dir "tests/"
   :compile "cd src/ && dotnet build"
   :run "cd src/ && dotnet run"
   :test "cd tests/ && dotnet test"
   :test-suffix "Tests")

  (projectile-register-project-type
   'dotnet
   #'mabo3n/projectile-dotnet-project-p
   :src-dir "./"
   :test-dir "./"
   :compile "dotnet build"
   :run "dotnet run"
   :test "dotnet test"
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

(spacemacs/set-leader-keys-for-major-mode 'csharp-mode
  ;; Replaces `spacemacs/jump-to-definition'
  ;; which is not working properly for csharp-mode
  "g g" #'omnisharp-go-to-definition)

(setq-default omnisharp-debug nil
              omnisharp-imenu-support t)

;;; snippet expansion

(add-hook 'csharp-mode-hook
          'mabo3n/prioritize-snippet-expansion-on-completion)

;;; evil text objects

(add-to-list 'load-path (expand-file-name "~/repos/csharpto"))
(require 'csharpto)

(csharpto-use-default-bindings-in-csharp-mode)

;;; misc

(defun mabo3n/csharp-set-shift-width ()
  (interactive)
  (setq-default evil-shift-width c-basic-offset))

(add-hook 'csharp-mode-hook
          'mabo3n/csharp-set-shift-width)

(provide 'init-csharp)
;;; init-csharp.el ends here
