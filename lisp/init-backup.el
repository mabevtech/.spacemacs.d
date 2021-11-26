;;; init-backup.el --- Custom backup functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'subr-x)
(require 'dash)
(require 'simple)
(require 'core-load-paths)

(defconst mabo3n/backup-files-remote-directory "d:env/"
  "Where `mabo3n/backup-files' moves files to.")

(defun mabo3n/backup-files (files &optional args)
  "Upload FILES to cloud under `mabo3n/backup-files-remote-directory'.

This builds and executes a rclone's copy command for each file in FILES.
ARGS are passed to each of these commands.

Backing up files outside of, or the whole `user-home-directory', is not allowed.

See URL `https://rclone.org/commands/rclone_copy/' for more info."
  (let ((shell-cmds))
    (dolist (file files)
      (catch 'continue
        (let* ((expanded-file (expand-file-name file))
               (dir-p (file-directory-p expanded-file))
               (file-path-relative-to-user-home
                (string-trim-left expanded-file
                                  (concat "^" (file-name-as-directory
                                               user-home-directory))))
               (destination-file
                (cond
                 ((not (string-prefix-p user-home-directory expanded-file))
                  (message "Cannot backup files outside user home directory \"%s\"."
                           expanded-file)
                  (throw 'continue nil))
                 ((string= user-home-directory expanded-file)
                  (message "Cannot backup the whole user home directory \"%s\"."
                           expanded-file)
                  (throw 'continue nil))
                 (t
                  (concat mabo3n/backup-files-remote-directory
                          (if dir-p
                              ;; **/foo/dir -> **/foo/dir/
                              (file-name-as-directory
                               file-path-relative-to-user-home)
                            ;; **/foo/file -> **/foo/
                            (file-name-directory
                             file-path-relative-to-user-home)))))))
          (setq shell-cmds
                (cons (format "rclone copy %s %s %s"
                              expanded-file
                              destination-file
                              (string-join args " "))
                      shell-cmds)))))
    (-> (string-join shell-cmds ";\n")
        message
        async-shell-command)))

(defun mabo3n/backup-recent-files (files &optional args)
  "Upload recent (24h) edited FILES to cloud.

Uses `mabo3n/backup-files' with ARGS."
  (interactive)
  (mabo3n/backup-files files (append args '("--max-age 24"))))

(defun mabo3n/backup-recent-emacs-files (&optional args)
  "Upload recent (24h) edited Emacs files to cloud.

\".spacemacs\" \"org/\" \".emacs.d/snippets/\"

Uses `mabo3n/backup-recent-files' with ARGS."
  (interactive)
  (mabo3n/backup-recent-files
   '(".spacemacs" "org/" ".emacs.d/private/") args))

(defun mabo3n/backup-recent-dotfiles (&optional args)
  "Upload recent (24h) edited dotfiles to cloud.

\".spacemacs\" \"org/\" \".emacs.d/private/\"
\".gitconfig\"
\".bashrc\" \".bash_profile\" \".bash_aliases\"
\".config/nvim/\" \".vimrc\" \".vim/\"

Uses `mabo3n/backup-recent-files' with ARGS."
  (interactive)
  (mabo3n/backup-recent-files
   '(".spacemacs" "org/" ".emacs.d/private/"
     ".gitconfig"
     ".bashrc" ".bash_profile" ".bash_aliases"
     ".config/nvim/" ".vimrc" ".vim/")
   args))

(provide 'init-backup)
;;; init-backup.el ends here
