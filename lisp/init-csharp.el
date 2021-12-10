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

(defun mabo3n/csharp--get-function-region
    (&optional include-around)
  "TODO: docstring"
  (let ((function-header-regexp
         (concat "\\(?1:\\(?:^[[:space:]]*\n\\)\\)"
                 "\\(?2:\\(?3:[[:space:]]*\\)[[:alpha:]].*[^;\n]\\)"
                 "\\(?:\n\\3[[:space:]]*[^[:space:]\n].*\\)\\{0,5\\}"
                 "\\(?:\n[[:space:]]+\\)?"
                 "\\(?:\\(?4:{\\)[[:space:]]*$\\|\\(?4:=>\\)\\)")))
    (save-excursion
      (let* ((p (point))
             (match-data
              (and (re-search-backward function-header-regexp nil t)
                   (match-data))))
        ;; TODO what if there's no class/namespace and match-data is nil?

        ;; `re-search-backward' only matches pattern ending BEFORE `point'.
        ;; If we're under a function header, the PREVIOUS function (or class)
        ;; header will be matched instead. So we try to match the next one
        ;; and, if it starts before previous point, that's the one we want.
        (unless (and (goto-char (point-at-eol)) ; avoid the same pattern FIXME: need to go after optional empty lines
                     (re-search-forward function-header-regexp nil t)
                     (<= (match-beginning 0) p))
          (goto-char p)
          (set-match-data match-data))
        (when-let ((beg-empty-lines (match-beginning 1))
                   (beg-header-line (match-beginning 2))
                   (end-header (match-end 4))
                   (indent-string (match-string-no-properties 3))
                   (open-scope-string (match-string-no-properties 4)))
          (let* ((lambda-exp-p (string= open-scope-string "=>"))
                 (end-of-scope-regexp
                  (if lambda-exp-p
                      ;; FIXME find last before indent shorter than functions'.
                      ;; this is matching any statement
                      "\\(?1:;[[:space:]]*\n\\)\\(?2:[[:space:]]*\n\\)*"
                    (concat "\\(?1:^"
                            indent-string
                            "}[[:space:]]*\n\\)\\(?2:[[:space:]]*\n\\)*")))
                 (end (and (re-search-forward end-of-scope-regexp nil t)
                           (if include-around
                               (match-end 0)
                             (match-end 1))))
                 (beg (if (and include-around
                               ;; Mimicking "word" behavior:
                               ;; If no empty spaces after object,
                               ;; include previous ones instead.
                               (not (match-string 2)))
                          beg-empty-lines
                        beg-header-line)))
            (list beg end)))))))

(evil-define-text-object evil-inner-csharp-function (count &optional beg end type)
  (mabo3n/csharp--get-function-region nil))
(define-key evil-inner-text-objects-map "f" 'evil-inner-csharp-function)
(evil-define-text-object evil-a-csharp-function (count &optional beg end type)
  (mabo3n/csharp--get-function-region t))
(define-key evil-outer-text-objects-map "f" 'evil-a-csharp-function)


;;; misc

(defun mabo3n/csharp-set-shift-width ()
  (interactive)
  (setq-default evil-shift-width c-basic-offset))

(add-hook 'csharp-mode-hook
          'mabo3n/csharp-set-shift-width)

(provide 'init-csharp)
;;; init-csharp.el ends here
