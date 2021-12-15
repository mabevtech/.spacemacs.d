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

(defun mabo3n/csharp--get-function-region
    (&optional include-around)
  "TODO: docstring"
  (let ((previous-empty-lines-group 1)
        (function-header-group 2)
        (function-header-indent-group 3)
        (function-open-delimiter-group 4)
        (function-header-regexp
         (concat "\\(?:\\(?:[^[:space:]\n]\\|\\`\\)[[:space:]]*\n\\)"
                 "\\(?1:\\(?:[[:space:]]*\n\\)*\\)"
                 "\\(?2:\\(?3:[[:space:]]*\\)[[:alpha:]].*[^;\n]\\)"
                 "\\(?:\n\\3[[:space:]]*[^[:space:]\n].*\\)\\{0,5\\}"
                 "\\(?:\n[[:space:]]+\\)?"
                 "\\(?:\\(?4:{\\)[[:space:]]*$\\|\\(?4:=>\\)\\)")))
    (catch 'region
      (save-excursion
       (let* ((p (point))
              (prev-fun-match-data
               (and (re-search-backward function-header-regexp nil t)
                    (match-data)))
              (next-fun-match-data
               (and (goto-char (or (match-end function-header-group)
                                   ;; go to beg of empty lines / scope openning to
                                   ;; match next fun including all whitespaces before it
                                   (and (re-search-backward "^[[:space:]]*\n" nil t)
                                        (re-search-backward "[^[:space:]][[:space:]]*\n" nil t)
                                        (1+ (match-end 0)))
                                   (and (re-search-backward "[^[:space:]][[:space:]]*\\(?:\n[[:space:]]+\\)?{[[:space:]]*\n" nil t)
                                        (1+ (match-end 0)))
                                   (point-at-bol)))
                    (re-search-forward function-header-regexp nil t)
                    (match-data))))

         (cond
          ((and next-fun-match-data
                (>= p (match-beginning function-header-group)))
           (set-match-data next-fun-match-data))
          ((and next-fun-match-data
                (>= p (match-beginning previous-empty-lines-group)))
           (if include-around
               (set-match-data next-fun-match-data)
             (throw 'region `(,(match-beginning previous-empty-lines-group)
                              ,(match-end       previous-empty-lines-group)))))
          (prev-fun-match-data
           (set-match-data prev-fun-match-data))
          (t
           (throw 'region '())))

         (when-let ((beg-empty-lines (match-beginning 1))
                    (beg-header-line (match-beginning 2))
                    (end-header (match-end 4))
                    (indent-string (match-string-no-properties 3))
                    (open-scope-string (match-string-no-properties 4)))
           ;; This assumes there's no other declarations between functions
           (goto-char end-header)
           (let* ((next-empty-lines-group 7)
                  (lambda-exp-p (string= open-scope-string "=>"))
                  (end-of-scope-regexp
                   (if lambda-exp-p
                       ;; FIXME find last before indent shorter than functions'.
                       ;; this is matching any statement
                       "\\(?1:;[[:space:]]*\n\\)\\(?7:\\(?:[[:space:]]*\n\\)*\\)"
                     (concat "\\(?1:^"
                             indent-string
                             "}[[:space:]]*\n\\)\\(?7:\\(?:[[:space:]]*\n\\)*\\)"))))

             (when (re-search-forward end-of-scope-regexp nil t)
               (if (>= p (match-beginning next-empty-lines-group))
                   (throw 'region `(,(match-beginning next-empty-lines-group)
                                    ,(match-end       next-empty-lines-group)))
                 (if (>= p beg-header-line)
                     (throw 'region
                         `(,(if (and include-around
                                     (not (> (length (match-string
                                                      next-empty-lines-group))
                                             0)))
                                beg-empty-lines
                              beg-header-line)
                           ,(if include-around
                                (match-end 0)
                              (match-end 1))))
                   (throw 'region `(,beg-empty-lines ,(match-end 1))))
                 ))))
         )))))

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
