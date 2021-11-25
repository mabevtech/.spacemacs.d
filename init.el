;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(
     osx
     helm
     (org :variables
          org-enable-github-support t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (docker :variables
             docker-dockerfile-backend 'lsp)
     yaml
     restclient
     themes-megapack
     (lsp :variables
          ;; lsp-lens-enable t
          lsp-headerline-breadcrumb-enable nil
          )
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      syntax-checking-enable-tooltips t)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      ;; auto-completion-complete-with-key-sequence '"jk"
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-idle-delay 0.2
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-use-company-box t
                      auto-completion-enable-sort-by-usage nil)
     multiple-cursors
     version-control
     git

     markdown
     html
     chrome

     emacs-lisp
     scheme

     ;; Requires omnisharp-emacs which requires mono
     ;; see https://github.com/OmniSharp/omnisharp-emacs
     ;; Requires msbuild for solutions (and a dotnet restore on project root)
     csharp

     (python :variables
             python-backend 'anaconda
             ;; python-lsp-server 'pyls
             python-test-runner 'pytest)
     ;; ipython-notebook

     (javascript :variables
                 javascript-backend 'lsp
                 javascript-lsp-linter nil
                 javascript-import-tool 'import-js
                 javascript-fmt-on-save t
                 node-add-modules-path t
                 js2-mode-show-strict-warnings nil
                 js2-mode-show-parse-errors nil
                 js2-basic-offset 4
                 js-indent-level 4 ;; JSON file indentation
                 web-mode-code-indent-offset 4
                 web-mode-indent-style 4)
     import-js
     react
     )

   ;; If you need some configuration for these packages, then
   ;; consider creating a layer. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(exec-path-from-shell
     kaolin-themes
     ;; company-tabnine
     ag
     add-node-modules-path
     cheat-sh)

   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only)
  )

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-read-process-output-max (* 1024 1024)
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-startup-buffer-show-version t
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists nil
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-show-startup-list-numbers nil
   dotspacemacs-startup-buffer-multi-digit-delay 0.4
   dotspacemacs-new-empty-buffer-major-mode 'text-mode
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-scratch-buffer-persistent t
   dotspacemacs-scratch-buffer-unkillable nil
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '(doom-nord-light doom-moonlight)
   dotspacemacs-mode-line-theme '(spacemacs
                                  :separator arrow
                                  :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Meslo LG S DZ for Powerline"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ", "
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-undecorated-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-scroll-bar-while-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-activate-smartparens-mode t
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-frame-title-format "Emacs"
   dotspacemacs-icon-title-format nil
   dotspacemacs-show-trailing-whitespace t
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-use-clean-aindent-mode t
   dotspacemacs-use-SPC-as-y t
   dotspacemacs-swap-number-row nil
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil
   dotspacemacs-home-shorten-agenda-source nil
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))


(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (defun projectile-dotnet-solution-p ()
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
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun mabo3n/helm-yas ()
  "Wrapper of `spacemacs/helm-yas'.

Workaround to region on visual state mode not working with yasnippets
https://github.com/emacs-evil/evil/issues/254#issuecomment-309839802."
  (interactive)
  (if (not (evil-visual-state-p))
      (spacemacs/helm-yas)
    (let ((visual-line-selection-p (eq evil-visual-selection 'line))
          (point-on-region-end-p (> (point) (mark))))
      (setq yas-wrap-around-region ?y
            evil-this-register ?y)
      (call-interactively #'evil-substitute)
      (evil-set-register ?y (if visual-line-selection-p
                                (string-trim-right (evil-get-register ?y) "\n")
                              (evil-get-register ?y)))
      (let* ((beg (point))
             (snippet-chosen-p (spacemacs/helm-yas)))
        (unless snippet-chosen-p
          (evil-paste-from-register ?y)
          (evil-normal-state)
          (evil-visual-state)
          (goto-char beg)
          (when point-on-region-end-p (exchange-point-and-mark)))
        snippet-chosen-p))))

(defun evil-yank-visual-state (beg end &rest args)
  "Yank then restore point (i.e. <y> <gv> <escape>).

Forward BEG, END and ARGS to `evil-yank'"
  (interactive "r")
  (apply #'evil-yank beg end args)
  (evil-visual-restore)
  (evil-exit-visual-state))

(defun mabo3n/org-setup ()
  (add-hook 'org-mode-hook (lambda () (setq show-trailing-whitespace t)))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda #1="")
            (alltodo #1#)))))

  ;;; org-capture stuff

  ;; Start org-capture in insert state. (adding to end of list cause
  ;; `spacemacs//org-capture-start' manually sets to normal state)
  (add-hook 'org-capture-mode-hook 'evil-insert-state 1)

  (defconst mabo3n/default-tasks-file (expand-file-name "tasks.org" org-directory)
    "Default tasks file.")
  (defconst mabo3n/default-tasks-headline "New"
    "Default tasks headline in tasks file.")
  (defvar mabo3n/tasks-file mabo3n/default-tasks-file
    "File to store org-capture task entries.")
  (defvar mabo3n/tasks-headline mabo3n/default-tasks-headline
    "Headline to store org-capture task entries.")

  (defun mabo3n/org-capture-file+headline-function (&optional prompt-defaults)
    "Captures into `mabo3n/tasks-file' within `mabo3n/tasks-headline'.

This provides an alternative for org-capture's file+headline target
with dynamic files and headlines. Code is adapted from file+headline
option code in `org-capture-set-target-location'.

If PROMPT-DEFAULTS is non-`nil', prompts for the file with
`mabo3n/default-tasks-file' selected, and defaults to
`mabo3n/default-tasks-headline' as headline (which can be changed
inside the org-capture buffer with `org-capture-refile')."
    (let ((buffer
           (org-capture-target-buffer
            (or (and prompt-defaults (helm-read-file-name
                                      "File: "
                                      ;; :test (apply-partially #'string-suffix-p ".org")
                                      :initial-input mabo3n/default-tasks-file))
                mabo3n/tasks-file)))
          (headline (or (and prompt-defaults mabo3n/default-tasks-headline)
                        mabo3n/tasks-headline)))
      (set-buffer buffer)
	    (unless (derived-mode-p 'org-mode)
	      (org-display-warning
	       (format "Capture requirement: switching buffer %S to Org mode"
		             (current-buffer)))
	      (org-mode))
	    (org-capture-put-target-region-and-position)
	    (widen)
	    (goto-char (point-min))
      (if (re-search-forward (format org-complex-heading-regexp-format
					                           (regexp-quote headline))
				                     nil t)
	        (beginning-of-line)
	      (goto-char (point-max))
	      (unless (bolp) (insert "\n"))
	      (insert "* " headline "\n")
	      (beginning-of-line 0))))

  (setq org-capture-templates
        '(("t" "Task" entry
           (function mabo3n/org-capture-file+headline-function)
           "* TODO %?\n  %u\n  %a" :clock-resume t)

          ("T" "Task (prompt)" entry
           (function (lambda () (mabo3n/org-capture-file+headline-function t)))
           "* TODO %?\n  %u\n  %a" :clock-resume t)

          ("n" "Note" entry (file "")  ;; "" => `org-default-notes-file'
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ))
  )

(defun mabo3n/enable-auto-reload-dir-locals-vars-on-save ()
  "From: https://emacs.stackexchange.com/a/13096."

  (defun mabo3n/reload-dir-locals-for-current-buffer ()
    "Reload dir-locals for the current buffer."
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))

  (defun mabo3n/reload-dir-locals-for-all-buffer-in-this-directory ()
    "Reload dir-locals for every buffer sharing `default-directory' with current's."
    (interactive)
    (let ((dir default-directory))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (equal default-directory dir)
            (mabo3n/reload-dir-locals-for-current-buffer))))))

  (add-hook 'emacs-lisp-mode-hook
            (defun enable-autoreload-for-dir-locals ()
              (when (and (buffer-file-name)
                         (equal dir-locals-file
                                (file-name-nondirectory (buffer-file-name))))
                (add-hook 'after-save-hook
                          #'mabo3n/reload-dir-locals-for-all-buffer-in-this-directory
                          nil t)))))

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

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Use a box cursor on insert/hybrid modes instead of bar one
  (setq-default evil-insert-state-cursor (list (car evil-insert-state-cursor) 'box)
                evil-hybrid-state-cursor (list (car evil-hybrid-state-cursor) 'box))

  (mabo3n/org-setup)

  (mabo3n/enable-auto-reload-dir-locals-vars-on-save)

  (setq-default
    ;; Turn flycheck on automatically for all global modes
    flycheck-global-modes t
    ;; Almost immediately pop up errors/warnings when the cursor enters a problematic expression
    flycheck-display-errors-delay 1.1
    ;; but dismiss them right after (just a quick peek)
    flycheck-pos-tip-timeout 3
    ;; restrict python's syntax checking for flake8 only
    flycheck-disabled-checkers '(python-mypy python-pylint)
    )

  (setq-default
    ;; Automatically check for version control changes to update mode line
    auto-revert-check-vc-info t
    ;; every this seconds
    auto-revert-timer 4
    )

  ;; Do not wait that much the next avy timer char
  (setq-default avy-timeout-seconds 0.2)

  ;; Do not auto add backslashes when inserting single quotes
  (setq-default sp-escape-quotes-after-insert nil)

  ;; Make sure that emacs will have $PATH variables from default shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  (setq garbage-collection-messages t)

  ;; Always use company-mode for completion (instead of auto-complete)
  (global-company-mode)

  ;; Add custom company backends
  ;; (add-to-list 'company-backends #'company-tabnine)
  ;; https://github.com/syl20bnr/spacemacs/issues/10638#issuecomment-386519064
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))

  ;; No fringes ~ on empty lines
  (spacemacs/toggle-vi-tilde-fringe-off)

  ;; Enable Omnisharp's detailed log buffer
  ;; (setq-default omnisharp-debug t)

  ;; Prevent evil from overriding some edebug keys (e.g. c, i, n)
  (add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

  (add-hook 'python-mode-hook
            (lambda ()
              ;; Override 'fixed value so regions work with snippets ($0)
              (with-eval-after-load 'yasnippet (setq yas-indent-line 'auto))
              (setq evil-shift-width 4)))

  ;; Make evil-indent use 4 spaces under csharp-mode
  (add-hook 'csharp-mode-hook
            (lambda ()
              (setq evil-shift-width c-basic-offset)))

  (defun switch-to-omnisharp-log-buffer ()
    (interactive)
    (switch-to-buffer "*omnisharp-log*"))
  (defun switch-to-omnisharp-log-buffer-other-window ()
    (interactive)
    (switch-to-buffer-other-window "*omnisharp-log*"))
  (spacemacs/set-leader-keys-for-major-mode 'csharp-mode
    "s l" #'switch-to-omnisharp-log-buffer
    "s L" #'switch-to-omnisharp-log-buffer-other-window)

  (setq omnisharp-imenu-support t)

  ;; Workaround to fix google-translate-at-point
  ;; https://github.com/atykhonov/google-translate/issues/52#issuecomment-470756933
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))

  ;; ------ Key bindings ------
  (define-key evil-visual-state-map (kbd "y") #'evil-yank-visual-state)
  (define-key evil-insert-state-map (kbd "C-y") #'yank)

  ;; Trigger auto completion
  (define-key evil-insert-state-map (kbd "C-SPC") #'company-manual-begin)
  (define-key evil-hybrid-state-map (kbd "C-SPC") #'company-manual-begin)
  ;; Open yasnippets menu
  (define-key evil-insert-state-map (kbd "C-;") #'mabo3n/helm-yas)
  (define-key evil-normal-state-map (kbd "C-;") #'mabo3n/helm-yas)
  (define-key evil-visual-state-map (kbd "C-;") #'mabo3n/helm-yas)
  (define-key evil-hybrid-state-map (kbd "C-;") #'mabo3n/helm-yas)

  ;; Use regex searches by default
  (global-set-key (kbd "C-s") #'isearch-forward-regexp)
  (global-set-key (kbd "C-r") #'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") #'isearch-forward)
  (global-set-key (kbd "C-M-r") #'isearch-backward)

  ;; Bind tab key to autocomplete

  ;; (define-key evil-insert-state-map (kbd "<tab>") 'hippie-expand)
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (evil-define-key '(insert normal) org-mode-map (kbd "<tab>") 'org-cycle)))

  ;; (dolist (mode-map '(csharp-mode-map
  ;;                     python-mode-map))
  ;;   (evil-define-key 'insert mode-map (kbd "<tab>") #'hippie-expand))

  (evil-define-key (list 'insert 'hybrid) csharp-mode-map (kbd "<tab>") #'hippie-expand)
  (evil-define-key (list 'insert 'hybrid) python-mode-map (kbd "<tab>") #'hippie-expand)
  (evil-define-key (list 'insert 'hybrid) markdown-mode-map (kbd "<tab>") #'hippie-expand)
  (evil-define-key (list 'insert 'hybrid) restclient-mode-map (kbd "<tab>") #'hippie-expand)

  ;; Treemacs
  (with-eval-after-load 'treemacs
    ;; Make navigation more like vim's
    (evil-define-key 'treemacs treemacs-mode-map
      (kbd "o s") #'treemacs-visit-node-vertical-split
      (kbd "o v") #'treemacs-visit-node-horizontal-split
      (kbd "o a s") #'treemacs-visit-node-ace-vertical-split
      (kbd "o a v") #'treemacs-visit-node-ace-horizontal-split
      ;; Make HhLl make some sense
      (kbd "H") #'treemacs-root-up
      (kbd "L") #'treemacs-root-down
      (kbd "h") #'treemacs-collapse-parent-node
      (kbd "l") #'treemacs-TAB-action
      ;; <r> to rename (<g r> to refresh)
      (kbd "r") #'treemacs-rename
      ;; <x> to delete
      (kbd "x") #'treemacs-delete))

  ;; Function to evaluate AND OUTPUT current line or selection on Python mode
  (defun python-shell-send-region-or-line-and-show-output
      (start end &optional msg)
    "Alternative to python-shell-send-region."
    (interactive
     (list (if mark-active (region-beginning) (line-beginning-position))
          (if mark-active (region-end) (line-end-position))
          t))
    (let* ((region (buffer-substring-no-properties start end))
           (process (python-shell-get-process-or-error msg)))
      (message "Sent: %s..." (string-trim region))
      (python-shell-send-string region process)))
  ;; Add function to major mode menu
  (spacemacs/set-leader-keys-for-major-mode
    'python-mode "o s l" 'python-shell-send-region-or-line-and-show-output)
  ;; Call it with S-return
  (evil-define-key (list 'insert 'hybrid 'normal) python-mode-map
    (kbd "<C-return>") 'python-shell-send-region-or-line-and-show-output)

  ;; <S-return> on restclient mode
  (evil-define-key (list 'insert 'hybrid 'normal) restclient-mode-map
    (kbd "<C-return>") 'restclient-http-send-current-stay-in-window)

  ;; <S-return> and <C-return> on scheme-mode

  ;; pq eh tao dificil fazer isso funcionar?
  ;; (defun with-current-line-as-default-region (FUNC)
  ;;   (let* ((start (if mark-active (region-beginning)
  ;;                   (line-beginning-position)))
  ;;          (end (if mark-active (region-end)
  ;;                 (line-end-position))))
  ;;     (apply FUNC start end)))

  ;; (evil-define-key 'normal scheme-mode-map
  ;;   (kbd "<C-return>") '(lambda ()
  ;;                         (interactive)
  ;;                         (with-current-line-as-default-region
  ;;                          #'geiser-eval-region)))

  (evil-define-key (list 'insert 'hybrid 'normal) scheme-mode-map
    (kbd "<C-return>") 'geiser-eval-region)

  (evil-define-key (list 'insert 'hybrid 'normal) scheme-mode-map
    (kbd "<S-return>") 'geiser-eval-region-and-go)

  ;; Genebra auto save file names are too long.
  ;; Name them with their sha1 instead
  (advice-add 'make-auto-save-file-name :around
              #'my-shorten-auto-save-file-name)
  (defun my-shorten-auto-save-file-name (&rest args)
    (let ((buffer-file-name
           (when buffer-file-name (sha1 buffer-file-name))))
      (apply args)))

  (spacemacs/set-leader-keys
    "x r b" #'regexp-builder
    "x r h" #'helm-regexp
    "T B"  #'toggle-scroll-bar
    "F F" #'make-frame
    "F <tab>" #'other-frame
    )

  ;;; Better usage of google suggest/translate

  (defun my-helm-google-suggest (&optional input)
    (interactive)
    (let ((input (or (and (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end)))
                     input))
          ;; This prevents some character repetition while typing
          (helm-input-idle-delay 0.4))
      (helm :sources 'helm-source-google-suggest
            :buffer "*helm google*"
            :input input
            :delayed t)))

  (defun my-helm-google-suggest-at-point ()
    (interactive)
    (let ((input (word-at-point t)))
      (unless input (message "Nothing at point."))
      (my-helm-google-suggest input)))

  (defun my-google-translate-query-translate (&rest args)
    (interactive)
    (if (use-region-p)
        (apply #'google-translate-at-point args)
      (apply #'google-translate-query-translate args)))

  (defun my-google-translate-query-translate-reverse (&rest args)
    (interactive)
    (if (use-region-p)
        (apply #'google-translate-at-point-reverse args)
      (apply #'google-translate-query-translate-reverse args)))

  (spacemacs/set-leader-keys
    "s G" (key-binding (kbd "SPC s g")) ;; bind +grep search to capital g
    "s g" nil                           ;; unbind +grep stuff from small g
    "s g g" #'my-helm-google-suggest
    "s g G" #'my-helm-google-suggest-at-point
    "s g t" (key-binding (kbd "SPC x g"))
    "s g t t" #'my-google-translate-query-translate
    "s g t T" #'google-translate-at-point
    "s g t v" #'my-google-translate-query-translate-reverse
    "s g t V" #'google-translate-at-point-reverse)
  (spacemacs/declare-prefix "s G" "grep")
  (spacemacs/declare-prefix "s g" "Google")
  (spacemacs/declare-prefix "s g t" "translate")

  (setq google-translate-pop-up-buffer-set-focus t)

  ;; Make query replace not stopping when encountering read-only text
  ;; This is useful while replacing on helm-ag-edit buffer
  (setq query-replace-skip-read-only t)

  ;; Quit evil-numbers-transient-state with Escape
  ;; (with-eval-after-load evil-numbers
  ;;   (define-key spacemacs/evil-numbers-transient-state/keymap
  ;;     (kbd "<escape>") (cdr (assq ?q spacemacs/evil-numbers-transient-state/keymap))))

  (setq company-tooltip-maximum-width 100)

  (define-key evil-inner-text-objects-map "f" #'evil-cp-inner-defun)
  (define-key evil-outer-text-objects-map "f" #'evil-cp-a-defun)

  ;; (evil-make-overriding-map Info-mode-map nil)
  ;; (add-hook 'Info-mode-hook #'evil-normalize-keymaps)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(google-translate-enable-ido-completion t t)
 '(google-translate-show-phonetic t t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(org-agenda-files
   '("/Users/mabo3n/repos/symptoms/README.org" "/Users/mabo3n/org/"))
 '(package-selected-packages
   '(reveal-in-osx-finder osx-trash osx-dictionary osx-clipboard launchctl company-box frame-local dash-functional nginx-mode zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode visual-fill-column winum white-sand-theme web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toxi-theme toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection string-edit sphinx-doc spaceline-all-the-icons memoize all-the-icons spaceline powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rjsx-mode reverse-theme restclient-helm restart-emacs rebecca-theme rainbow-delimiters railscasts-theme quickrun pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin poetry planet-theme pippel pipenv pyvenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode persistent-scratch password-generator paradox ox-gfm overseer orgit-forge orgit organic-green-theme org-superstar org-rich-yank org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink org-brain open-junk-file omtose-phellack-theme omnisharp oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http npm-mode nose nodejs-repl noctilux-theme naquadah-theme nameless mustang-theme multi-term multi-line shut-up monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme modus-themes mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-section madhat2r-theme macrostep lush-theme lsp-ui lsp-treemacs treemacs cfrs pfuture posframe lsp-python-ms lsp-pyright lsp-origami origami lorem-ipsum livid-mode skewer-mode live-py-mode link-hint light-soap-theme kaolin-themes json-navigator hierarchy js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide importmagic epc ctable concurrent deferred import-js grizzl impatient-mode simple-httpd hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-pydoc helm-purpose window-purpose imenu-list helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp lsp-mode spinner ht helm-ls-git helm-gitignore request helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode ham-mode html-to-markdown gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ fringe-helper git-gutter+ gh-md geiser gandalf-theme fuzzy forge markdown-mode magit ghub closql emacsql-sqlite emacsql treepy git-commit with-editor flymd flycheck-pos-tip flycheck-package package-lint flycheck-elsa flycheck flx-ido flx flatui-theme flatland-theme farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-collection annalist evil-cleverparens smartparens evil-args evil-anzu anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emr iedit clang-format projectile paredit list-utils pkg-info epl emmet-mode elisp-slime-nav editorconfig edit-server dumb-jump drag-stuff dracula-theme doom-themes dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat django-theme dired-quick-sort devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csharp-mode tree-sitter-langs tree-sitter-indent tree-sitter tsc company-web web-completion-data company-restclient restclient know-your-http-well company-quickhelp pos-tip company-anaconda company column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode chocolate-theme autothemer cherry-blossom-theme cheat-sh centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f ample-zen-theme ample-theme alect-themes aggressive-indent ag s dash afternoon-theme add-node-modules-path ace-window ace-link ace-jump-helm-line helm avy helm-core ac-ispell auto-complete popup which-key use-package pcre2el org-plus-contrib hydra lv hybrid-mode font-lock+ evil goto-chg dotenv-mode diminish bind-map bind-key async))
 '(reb-re-syntax 'string)
 '(safe-local-variable-values
   '((mabo3n/tasks-headline . "Tasks")
     (eval setq-local mabo3n/tasks-file
           (expand-file-name "README.org"))
     (mabo3n/tasks-headline "Tasks")
     (mabo3n/tasks-file
      (expand-file-name "README.org"))
     (projectile-project-type . dotnet-sln)
     (projectile-project-type quote dotnet-sln)
     (projectile-root-local ".")
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
