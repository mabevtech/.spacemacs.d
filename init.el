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
                      ;; auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-idle-delay nil
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
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
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
   dotspacemacs-mode-line-theme 'doom
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
   dotspacemacs-frame-title-format nil
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
  (add-to-list 'load-path (expand-file-name ".spacemacs.d/lisp/")))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (require 'init-emacs)
  (require 'init-frames)
  (require 'init-evil)
  (require 'init-org)
  (require 'init-yas)
  (require 'init-backup)
  (require 'init-google)
  (require 'init-elisp)
  (require 'init-python)
  (require 'init-csharp)

  ;; esc always quits
  ;; https://superuser.com/a/945245
  (define-key minibuffer-local-map [escape] #'keyboard-escape-quit)
  (define-key minibuffer-local-ns-map [escape] #'keyboard-escape-quit)
  (define-key minibuffer-local-completion-map [escape] #'keyboard-escape-quit)
  (define-key minibuffer-local-must-match-map [escape] #'keyboard-escape-quit)
  (define-key minibuffer-local-isearch-map [escape] #'keyboard-escape-quit)
  (global-set-key [escape] 'keyboard-escape-quit)
  ;; https://github.com/syl20bnr/spacemacs/issues/11989#issuecomment-502131534
  (which-key-define-key-recursively global-map [escape] #'ignore)
  (which-key-define-key-recursively evil-emacs-state-map [escape] #'ignore)

  (spacemacs/set-leader-keys "f ESC" #'ignore)

  ;; Improve modeline performance
  ;; https://github.com/seagle0128/doom-modeline/issues/37#issuecomment-435311367
  (setq inhibit-compacting-font-caches t)

  ;; No fringes ~ on empty lines
  (spacemacs/toggle-vi-tilde-fringe-off)

  (setq-default
    ;; Turn flycheck on automatically for all global modes
    flycheck-global-modes t
    ;; Almost immediately pop up errors/warnings when the cursor enters a problematic expression
    flycheck-display-errors-delay 1.1
    ;; but dismiss them right after (just a quick peek)
    flycheck-pos-tip-timeout 3
    )

  ;; Open COMMIT_EDITMSG buffer already in insert state
  (add-hook #'git-commit-mode-hook 'evil-insert-state)

  ;; Do not wait that much the next avy timer char
  (setq-default avy-timeout-seconds 0.2)

  ;; Do not auto add backslashes when inserting single quotes
  (setq-default sp-escape-quotes-after-insert nil)

  ;; Quit evil-numbers-transient-state with Escape
  ;; (with-eval-after-load evil-numbers
  ;;   (define-key spacemacs/evil-numbers-transient-state/keymap
  ;;     (kbd "<escape>") (cdr (assq ?q spacemacs/evil-numbers-transient-state/keymap))))

  (setq company-tooltip-maximum-width 100)

  ;; Always use company-mode for completion (instead of auto-complete)
  (global-company-mode)

  ;;; Key bindings

  ;; Trigger auto completion
  (define-key evil-insert-state-map (kbd "C-SPC") #'company-manual-begin)
  (define-key evil-hybrid-state-map (kbd "C-SPC") #'company-manual-begin)

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

  (evil-define-key (list 'insert 'hybrid 'normal) restclient-mode-map
    (kbd "<C-return>") 'restclient-http-send-current-stay-in-window)

  (evil-define-key (list 'insert 'hybrid 'normal) scheme-mode-map
    (kbd "<C-return>") 'geiser-eval-region)

  (evil-define-key (list 'insert 'hybrid 'normal) scheme-mode-map
    (kbd "<S-return>") 'geiser-eval-region-and-go)

  (spacemacs/set-leader-keys
    "x r b" #'regexp-builder
    "x r h" #'helm-regexp
    "T B"  #'toggle-scroll-bar)

  (evil-define-key (list 'insert 'hybrid) markdown-mode-map (kbd "<tab>") #'hippie-expand)
  (evil-define-key (list 'insert 'hybrid) restclient-mode-map (kbd "<tab>") #'hippie-expand)

  (defun mabo3n/helm-jump-in-buffer ()
    "Store `point' value then prompt."
    (interactive)
    (setq mabo3n/point-before-helm-jump-in-buffer (point))
    (lazy-helm/spacemacs/helm-jump-in-buffer))
  (spacemacs/set-leader-keys "s j" #'mabo3n/helm-jump-in-buffer)

  (defun mabo3n/set-evil-jump-previous-position ()
    "If `point' has changed, set an evil jump to its previous position."
    (when-let* ((prev-point mabo3n/point-before-helm-jump-in-buffer)
                (changedp (not (eq (point) prev-point))))
      (evil-set-jump prev-point)))
  (add-hook 'imenu-after-jump-hook #'mabo3n/set-evil-jump-previous-position)
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
 '(org-agenda-files '("/Users/mabo3n/org/"))
 '(package-selected-packages
   '(doom-modeline shrink-path reveal-in-osx-finder osx-trash osx-dictionary osx-clipboard launchctl company-box frame-local dash-functional nginx-mode zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode visual-fill-column winum white-sand-theme web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen undo-tree underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toxi-theme toc-org terminal-here tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon symbol-overlay sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection string-edit sphinx-doc spaceline-all-the-icons memoize all-the-icons spaceline powerline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode sass-mode rjsx-mode reverse-theme restclient-helm restart-emacs rebecca-theme rainbow-delimiters railscasts-theme quickrun pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prettier-js popwin poetry planet-theme pippel pipenv pyvenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode persistent-scratch password-generator paradox ox-gfm overseer orgit-forge orgit organic-green-theme org-superstar org-rich-yank org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-cliplink org-brain open-junk-file omtose-phellack-theme omnisharp oldlace-theme occidental-theme obsidian-theme ob-restclient ob-http npm-mode nose nodejs-repl noctilux-theme naquadah-theme nameless mustang-theme multi-term multi-line shut-up monokai-theme monochrome-theme molokai-theme moe-theme modus-vivendi-theme modus-operandi-theme modus-themes mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-section madhat2r-theme macrostep lush-theme lsp-ui lsp-treemacs treemacs cfrs pfuture posframe lsp-python-ms lsp-pyright lsp-origami origami lorem-ipsum livid-mode skewer-mode live-py-mode link-hint light-soap-theme kaolin-themes json-navigator hierarchy js2-refactor multiple-cursors js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide importmagic epc ctable concurrent deferred import-js grizzl impatient-mode simple-httpd hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-pydoc helm-purpose window-purpose imenu-list helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp lsp-mode spinner ht helm-ls-git helm-gitignore request helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme haml-mode gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gmail-message-mode ham-mode html-to-markdown gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ fringe-helper git-gutter+ gh-md geiser gandalf-theme fuzzy forge markdown-mode magit ghub closql emacsql-sqlite emacsql treepy git-commit with-editor flymd flycheck-pos-tip flycheck-package package-lint flycheck-elsa flycheck flx-ido flx flatui-theme flatland-theme farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-collection annalist evil-cleverparens smartparens evil-args evil-anzu anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emr iedit clang-format projectile paredit list-utils pkg-info epl emmet-mode elisp-slime-nav editorconfig edit-server dumb-jump drag-stuff dracula-theme doom-themes dockerfile-mode docker transient tablist json-mode docker-tramp json-snatcher json-reformat django-theme dired-quick-sort devdocs define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csharp-mode tree-sitter-langs tree-sitter-indent tree-sitter tsc company-web web-completion-data company-restclient restclient know-your-http-well company-quickhelp pos-tip company-anaconda company column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode chocolate-theme autothemer cherry-blossom-theme cheat-sh centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote blacken birds-of-paradise-plus-theme badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f ample-zen-theme ample-theme alect-themes aggressive-indent ag s dash afternoon-theme add-node-modules-path ace-window ace-link ace-jump-helm-line helm avy helm-core ac-ispell auto-complete popup which-key use-package pcre2el org-plus-contrib hydra lv hybrid-mode font-lock+ evil goto-chg dotenv-mode diminish bind-map bind-key async))
 '(reb-re-syntax 'string)
 '(safe-local-variable-values
   '((projectile-test-suffix-function lambda
                                      (_)
                                      "-tests")
     (mabo3n/tasks-headline . "Tasks")
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
