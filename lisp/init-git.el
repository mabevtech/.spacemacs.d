;;; init-git.el --- Git stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'git-link)

;;; git-link

(setq-default git-link-use-commit nil
              git-link-open-in-browser t)

(spacemacs/set-leader-keys
  "g l h" 'mabo3n/git-link-homepage-yank
  "g l H" 'mabo3n/git-link-homepage-open
  "g l c" 'mabo3n/git-link-commit-yank
  "g l C" 'mabo3n/git-link-commit-open
  "g l l" 'mabo3n/git-link-yank
  "g l L" 'mabo3n/git-link-open)

(defun mabo3n/git-link-homepage-yank ()
  "Call `git-link-homepage' with `git-link-open-in-browser' set to nil."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-homepage)))

(defun mabo3n/git-link-homepage-open ()
  "Call `git-link-homepage' with `git-link-open-in-browser' set to t."
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link-homepage)))

(defun mabo3n/git-link-commit-yank ()
  "Call `git-link-commit' with `git-link-open-in-browser' set to nil."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-commit)))

(defun mabo3n/git-link-commit-open ()
  "Call `git-link-commit' with `git-link-open-in-browser' set to t."
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link-commit)))

(defun mabo3n/git-link (toggle-use-commit)
  "Wrapper of `git-link'.

If TOGGLE-USE-COMMIT is non-nil, the behavior of `git-link-use-commit'
is reversed.
With a single \\[universal-argument], TOGGLE-USE-COMMIT is set to t.
Any extra \\[universal-argument] are forwarded to `git-link'."
  (interactive "P")
  (let ((git-link-use-commit (xor git-link-use-commit
                                  toggle-use-commit))
        (current-prefix-arg  (if (consp current-prefix-arg)
                                 (if (> (car current-prefix-arg) 4)
                                     (list (/ (car current-prefix-arg) 4))
                                   nil))))
    (call-interactively 'git-link)))

(defun mabo3n/git-link-yank ()
  "Call `mabo3n/git-link' with `git-link-open-in-browser' set to nil."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'mabo3n/git-link)))

(defun mabo3n/git-link-open ()
  "Call `mabo3n/git-link' with `git-link-open-in-browser' set to t."
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'mabo3n/git-link)))

;;; misc

;; Open COMMIT_EDITMSG buffer already in insert state
(with-eval-after-load 'git-commit
  (add-hook #'git-commit-mode-hook 'evil-insert-state))


;; Fix evil-surround getting in the way while trying to visually-select and
;; stage specific lines in magit-status buffer (by disabling the mode in there).
;; Regression: https://github.com/syl20bnr/spacemacs/issues/15448
(with-eval-after-load 'evil-surround
  (defun mabo3n/turn-off-evil-surround-mode ()
    "Turn off evil-surround-mode"
    (evil-surround-mode -1))
  (add-hook 'magit-status-mode-hook 'mabo3n/turn-off-evil-surround-mode))

(provide 'init-git)
;;; init-git.el ends here
