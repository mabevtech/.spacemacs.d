;;; init-org-roam.el --- Org roam config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

(with-eval-after-load 'org-roam
  (global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c n f") #'org-roam-node-find)
  (global-set-key (kbd "C-c n g") #'org-roam-graph)
  (global-set-key (kbd "C-c n i") #'org-roam-node-insert)
  (global-set-key (kbd "C-c n c") #'org-roam-capture)
  (global-set-key (kbd "C-c n a") #'org-roam-alias-add)
  (global-set-key (kbd "C-c n A") #'org-roam-alias-remove)
  (global-set-key (kbd "C-c n t") #'org-roam-tag-add)
  (global-set-key (kbd "C-c n T") #'org-roam-tag-remove)

  (setq org-roam-directory (expand-file-name "~/org/roam/")

        ;; Only useful displaying tags because I'm using a
        ;; vertical completion framework (helm)
        org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))

        org-roam-capture-templates
        `(("d" "default" plain "\n* TODO roam entry: ${title}%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              ,(concat "#+title: ${title}\n"
                                       "#+created:       %U\n"
                                       "#+last_modified: %U\n"
                                       "#+filetags:\n"))
           :unnarrowed t
           :immediate-finish t)))

  ;; Recommended
  (org-roam-db-autosync-mode)

  ;; Update "last_modified" date when saving buffer
  ;; https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321
  (defun mabo3n/org-roam-set-time-stamp-vars ()
    "Set time-stamp variables to auto update last_modified property."
    (when (derived-mode-p 'org-mode)
      (require 'time-stamp)
      (setq-local time-stamp-active t
                  time-stamp-line-limit 24
                  time-stamp-start "#\\+last_modified:[ ]*"
                  time-stamp-end "$"
                  time-stamp-format "\[%Y-%m-%d %3a %H:%M\]")))
  (defun mabo3n/org-roam-timestamp-on-save ()
    "Call `time-stamp' function if in `org-mode'."
    (when (derived-mode-p 'org-mode)
      (time-stamp)))
  (add-hook 'org-mode-hook #'mabo3n/org-roam-set-time-stamp-vars)
  (add-hook 'before-save-hook #'mabo3n/org-roam-timestamp-on-save))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
