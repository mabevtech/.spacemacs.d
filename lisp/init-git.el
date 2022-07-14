;;; init-git.el --- Git stuff -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Open COMMIT_EDITMSG buffer already in insert state
(with-eval-after-load 'git-commit
  (add-hook #'git-commit-mode-hook 'evil-insert-state))

;;; init-git.el ends here
