;;; init-backup.el --- Custom backup functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'subr-x)
;; (require 'dash)
;; (require 'simple)
;; (require 'core-load-paths)
;; (require 'core-dotspacemacs)

(defconst mabo3n/backup-files-remote-directory "d:env/"
  "Where `mabo3n/backup-files' moves files to.")

(defun mabo3n/backup-files--get-remote-path (file)
  "Get remote path for FILE."
  (let ((expanded-file (expand-file-name file)))
    (when (string-prefix-p user-home-directory expanded-file)
      (let ((dir-p (file-directory-p expanded-file))
            (file-path-relative-to-user-home
             (string-trim-left expanded-file
                               (concat "^" (file-name-as-directory
                                            user-home-directory)))))
        (concat mabo3n/backup-files-remote-directory
                (if dir-p
                    ;; **/foo/dir -> **/foo/dir/
                    (file-name-as-directory
                     file-path-relative-to-user-home)
                  ;; **/foo/file -> **/foo/
                  (file-name-directory
                   file-path-relative-to-user-home)))))))

(defun mabo3n/backup-files--build-backup-command (file &optional args)
  "Generate a shell command to backup FILE.

FILE may by a file path or a list of file paths.
ARGS is a list of string arguments forwarded to rclone."
  (let ((command
         (catch 'continue
           (let* ((f (or (and (consp file) (car file))
                         file))
                  (expanded-file
                   (if (stringp f) (expand-file-name f)
                     (message "File is not a string \"%s\"." f)
                     (throw 'continue nil)))
                  (destination-file
                   (cond
                    ((not (string-prefix-p user-home-directory expanded-file))
                     (message "Cannot backup file from outside user home directory \"%s\"."
                              expanded-file)
                     (throw 'continue nil))
                    ((string= user-home-directory expanded-file)
                     (message "Cannot backup the whole user home directory \"%s\"."
                              expanded-file)
                     (throw 'continue nil))
                    (t (mabo3n/backup-files--get-remote-path expanded-file)))))
             (unless destination-file
               (message "Couldn't get remote path for \"%s\"" expanded-file)
               (throw 'continue nil))
             (concat (format "rclone copy %s %s"
                             expanded-file
                             destination-file)
                     (and args " ")
                     (string-join args " "))))))
    (if (consp file)
        (let ((fn (apply-partially #'mabo3n/backup-files--build-backup-command
                                   (cdr file) args)))
          (if command
              (cons command (and (cdr file) (funcall fn)))
            (funcall fn)))
      command)))

(defun mabo3n/backup-files (files &optional args)
  "Upload FILES to cloud under `mabo3n/backup-files-remote-directory'.

This builds and executes a rclone's copy command for each file in FILES.
ARGS are passed to each of these commands.

Backing up files outside of, or the whole `user-home-directory', is not allowed.

See URL `https://rclone.org/commands/rclone_copy/' for more info."
  (-> (mabo3n/backup-files--build-backup-command files args)
      (string-join ";\n")
      message
      async-shell-command))

(defun mabo3n/backup-recent-files (files &optional args)
  "Upload recent (24h) edited FILES to cloud.

Uses `mabo3n/backup-files' with ARGS."
  (mabo3n/backup-files files (append args '("--max-age 24"))))

(defun mabo3n/backup-recent-dotfiles (&optional args)
  "Upload recent (24h) edited dotfiles to cloud.

\".gitconfig\"
\".bashrc\" \".bash_profile\" \".bash_aliases\"
\".config/nvim/\" \".vimrc\" \".vim/\"

Uses `mabo3n/backup-recent-files' with ARGS."
  (interactive)
  (mabo3n/backup-recent-files
   (mapcar (apply-partially 'concat (file-name-as-directory
                                     user-home-directory))
           '(".gitconfig"
             ".bashrc" ".bash_profile" ".bash_aliases"
             ".config/nvim/" ".vimrc" ".vim/"))
   args))

(defun mabo3n/backup-org-files (&optional args)
  "Upload org files to cloud.

Uses `mabo3n/backup-recent-files' with ARGS."
  (interactive)
  (mabo3n/backup-files
   `(,(expand-file-name "org/" user-home-directory)) args))


(defconst mabo3n/backup-dotspacemacs-default-commit-message
  "<auto commit>"
  "Default commit message used by `mabo3n/backup-dotspacemacs-changes'.")

(defun mabo3n/backup-dotspacemacs-files (&optional commit-msg)
  "Commit and push to remote all dotspacemacs file changes.

Optional COMMIT-MSG can be provided, using
`mabo3n/backup-dotspacemacs-default-commit-message' as the default one."
  (interactive (list
                (read-string
                 (format "Commit message (default %s):\n"
                         mabo3n/backup-dotspacemacs-default-commit-message))))
  (let* ((default-directory dotspacemacs-directory)
         (msg (or commit-msg
                  mabo3n/backup-dotspacemacs-default-commit-message)))
    (-> (format (concat "git add ."
                        " && git commit -m \"%s\""
                        " && git push")
                msg)
        message
        async-shell-command)))

(provide 'init-backup)
;;; init-backup.el ends here
