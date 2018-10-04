;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     csv
     ;; php
     better-defaults
     osx
     ;; csv
     ;; python
     ;; vimscript
     sql
     coffeescript
     ;; (c-c++ :variables c-c++-enable-clang-support t)
     ;; clojure
     docker
     markdown
     html
     yaml
     (node :variables
           node-add-modules-path t)
     ;; lsp
     prettier
     (javascript :variables
                 ;; javascript-backend 'lsp
                 ;; javascript-disable-tern-port-files t
                 node-add-modules-path t)
     (ruby :variables
           ruby-version-manager 'rvm
           ruby-enable-enh-ruby-mode t)
     ruby-on-rails
     (react :variables javascript-fmt-tool 'prettier)
     ;; ess
     ;; restclient
     ;; (go :variables
     ;;     ;; go-use-gometalinter t
     ;;     gofmt-command "goimports"
     ;;     go-tab-width 4)
     copy-as-format
     (org :variables
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-want-todo-bindings t
          org-enable-hugo-support t)
     spacemacs-org
     ;; plantuml
     ;; shell-scripts
     hero
     prodigy
     (shell :variables
            shell-default-shell 'multi-term
            shell-enable-smart-eshell t
            shell-default-position 'full
            shell-default-full-span t)
     ;; helm
     (ivy :variables ivy-enable-advanced-buffer-information nil)
     ;; (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     ;; ycmd
     ;; lsp
     (auto-completion :variables
                      ;; auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-tab-key-behavior 'cycle
                      ;; auto-completion-return-key-behavior nil
                      auto-completion-complete-with-key-sequence-delay 0.5
                      auto-completion-complete-with-key-sequence "jk")

     ;; spell-checking
     syntax-checking
     git
     github
     (version-control :variables
                      version-control-global-margin t
                      version-control-diff-side 'right
                      version-control-diff-tool 'diff-hl)
     search-engine
     tmux
     ;; vinegar
     treemacs
     ;; themes-megapack
     (spacemacs-layouts :variables layouts-enable-autosave nil layouts-autosave-delay 300)
     ;; (ibuffer :variables ibuffer-group-buffers-by 'projects)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      osx-clipboard
                                      nord-theme
                                      read-aloud
                                      format-sql
                                      ;; company-flow
                                      ;; emamux
                                      ;; (all-the-icons :location (recipe :fetcher github :repo "ubolonton/all-the-icons.el" :branch "font-lock-fix"))
                                      all-the-icons-dired
                                      all-the-icons-ivy
                                      apib-mode
                                      ;; kanban
                                      evil-terminal-cursor-changer
                                      (eterm-256color :location (recipe :fetcher github
                                                                        :repo "dieggsy/eterm-256color"
                                                                        :branch "devel"))
                                      ;; rebecca-theme
                                      ;; eshell-git-prompt
                                      doom-themes
                                      rubocopfmt
                                      ;; ewmctrl
                                      ;; mplayer-mode
                                      ;; spaceline-all-the-icons
                                      ;; nodejs-repl
                                      ;; company-shell
                                      ;; fzf
                                      ;; js-format
                                      ;; atom-dark-theme
                                      ;; etags-select
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(evil-escape)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes (if (display-graphic-p)
                           '(nord spacemacs-dark spacemacs-light)
                         '(doom-vibrant wombat doom-nord-light))
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme (if (display-graphic-p)
                                    '(spacemacs :separator wave :separator-scale 1.0)
                                  '(spacemacs :separator alternate :separator-scale 1.2))
   ;; dotspacemacs-mode-line-theme
   ;; dotspacemacs-mode-line-theme '(vanilla)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '(
                               ;; "Source Code Pro"
                               ;; "Fira Code"
                               "FuraCode Nerd Font"
                               ;; "Monaco"
                               ;; "Inconsolata"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.4)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 500

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

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
  ;; (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer-elpa-archives)
  ;; (push '(use-package . "melpa-stable") package-pinned-packages)
  (setq custom-file "~/.spacemacs.custom.el")
  (load custom-file)
  )

(defun user-config-gui ()
  "Configuration function for Graphical UI"
  ;; (setq powerline-default-separator 'contour)
  ;; (setq powerline-default-separator 'alternate)
  ;; (with-eval-after-load "treemacs"
  ;;   (treemacs-define-custom-icon
  ;;    (propertize " "
  ;;                'display (create-image "~/projects/icons/ruby.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-selected (create-image "~/projects/icons/ruby.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-unselected (create-image "~/projects/icons/ruby.png" 'png nil :ascent 'center :scale 1)
  ;;                )
  ;;    "rb" "ru" "Gemfile" "Gemfile.lock" "rake")
  ;; (treemacs-define-custom-icon
  ;;    (propertize " "
  ;;                'display (create-image "~/projects/icons/Makefile.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-selected (create-image "~/projects/icons/Makefile.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-unselected (create-image "~/projects/icons/Makefile.png" 'png nil :ascent 'center :scale 1)
  ;;                )
  ;;    "Makefile")
  ;;   (treemacs-define-custom-icon
  ;;    (propertize " "
  ;;                'display (create-image "~/projects/icons/docker.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-selected (create-image "~/projects/icons/docker.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-unselected (create-image "~/projects/icons/docker.png" 'png nil :ascent 'center :scale 1)
  ;;                )
  ;;    "Dockerfile")
  ;;   (treemacs-define-custom-icon
  ;;    (propertize " "
  ;;                'display (create-image "~/projects/icons/apib.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-selected (create-image "~/projects/icons/apib.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-unselected (create-image "~/projects/icons/apib.png" 'png nil :ascent 'center :scale 1)
  ;;                )
  ;;    "apib")
  ;;   (treemacs-define-custom-icon
  ;;    (propertize " "
  ;;                'display (create-image "~/projects/icons/conf.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-selected (create-image "~/projects/icons/conf.png" 'png nil :ascent 'center :scale 1)
  ;;                'img-unselected (create-image "~/projects/icons/conf.png" 'png nil :ascent 'center :scale 1)
  ;;                )
  ;;    "yml"
  ;;    "development"
  ;;    "test"
  ;;    "production"
  ;;    "yaml"
  ;;    "DS_Store"
  ;;    "properties"
  ;;    "conf"
  ;;    "config"
  ;;    "gitignore"
  ;;    "gitconfig"
  ;;    "gitmodules"
  ;;    "ini"
  ;;    "xdefaults"
  ;;    "xresources"
  ;;    "terminalrc"
  ;;    "org"
  ;;    "toml"
  ;;    "babelrc"
  ;;    "eslintrc"
  ;;    "eslintignore"
  ;;    "eslintcache"
  ;;    "dockerignore"
  ;;    "flowconfig"
  ;;    "projectile"
  ;;    "hgignore"
  ;;    "ruby-version"
  ;;    "buildpacks"
  ;;    "danger-whitelist"
  ;;    "npmignore"
  ;;    "npmrc"
  ;;    "prettierrc"
  ;;    "mention-bot"
  ;;    ))
  )

(defun user-config-tui ()
  "Configuration function for Terminal UI"
  (setq powerline-default-separator 'utf-8)
  ;; clipboard for emacs version >= 26
  (use-package osx-clipboard
     :config
     (progn
       (osx-clipboard-mode +1)
       (diminish 'osx-clipboard-mode)))
  (evil-leader/set-key "x t m" 'emamux:send-region)

  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)

  ;; tmux
  (defun tmux-smart-shell ()
    "Invoke a tmux panel"
    (interactive)
    (let ((command "tmux split-window -p 25"))
      (if (projectile-project-p)
          (projectile-with-default-dir (projectile-project-root)
            (shell-command command))
        (shell-command command))))
  (evil-leader/set-key "t '" 'tmux-smart-shell)

  ;; (setq treemacs-no-png-images t)
  ;; (defun my-treemacs-hash-icons ()
  ;;   "Create and define all icons-related caches, hashes and stashes."
  ;;   (setq-local treemacs-icons-hash (make-hash-table :size 100 :test #'equal))
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "vim")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "svg")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "md" "markdown")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "js")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "jsx")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "css")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "png" "pdf" "jpg")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "ico")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "html")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "clj")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "cljs")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "go")
  ;;   (treemacs-define-custom-icon
  ;;    (propertize "" 'face 'font-lock-keyword-face)
  ;;    "yml"
  ;;    "yaml"
  ;;    "DS_Store"
  ;;    "properties"
  ;;    "conf"
  ;;    "config"
  ;;    "gitignore"
  ;;    "gitconfig"
  ;;    "ini"
  ;;    "xdefaults"
  ;;    "xresources"
  ;;    "terminalrc"
  ;;    "org"
  ;;    "toml")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "rb" "ruby")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "zsh" "bash" "sh")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "py")
  ;;   (treemacs-define-custom-icon (propertize "" 'face 'font-lock-keyword-face) "json")
  ;;   treemacs-icons-hash)

;;   (defun my-treemacs--create-file-button-strings (path prefix parent depth)
;;     "Return the text to insert for a file button for PATH.
;; PREFIX is a string inserted as indentation.
;; PARENT is the (optional) button under which this one is inserted.
;; DEPTH indicates how deep in the filetree the current button is."
;;     (my-treemacs-hash-icons)
;;     (list
;;      prefix
;;      (ht-get treemacs-icons-hash
;;              (-> path (treemacs--file-extension) (downcase))
;;              treemacs-icon-fallback)
;;      (propertize (file-name-nondirectory path)
;;                  'button '(t)
;;                  'category 'default-button
;;                  'help-echo nil
;;                  'keymap nil
;;                  :default-face 'treemacs-git-unmodified-face
;;                  :state 'file-node-closed
;;                  :path path
;;                  :parent parent
;;                  :depth depth)))

;;   (advice-add 'treemacs--create-file-button-strings :override #'my-treemacs--create-file-button-strings )

;;   (with-eval-after-load "treemacs"
;;     (setq
;;      treemacs-icon-tag-node-open-txt   (propertize "▾ " 'face 'font-lock-keyword-face)
;;      treemacs-icon-tag-node-closed-txt (propertize "▸ " 'face 'font-lock-keyword-face)
;;      treemacs-icon-open-text   (propertize "▾ " 'face 'font-lock-keyword-face)
;;      treemacs-icon-closed-text (propertize "▸ " 'face 'font-lock-keyword-face)
;;      treemacs-icon-tag-leaf-txt (propertize "- " 'face 'font-lock-keyword-face)
;;      treemacs-icon-fallback-text (propertize " " 'face 'font-lock-keyword-face)

;;      treemacs-icon-open-png   (propertize "▾ " 'face 'font-lock-keyword-face)
;;      treemacs-icon-closed-png (propertize "▸ " 'face 'font-lock-keyword-face)
;;      treemacs-icon-text (propertize " " 'face 'font-lock-keyword-face)))

  ;; cursor shape
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

(defun user-config-general ()
  "General configurations"
  (setq mode-require-final-newline t)
  (setq require-final-newline t)
  ;; (setq large-file-warning-threshold nil)
  (setq tags-add-tables nil)
  ;; (setq gc-cons-threshold (* 100 1024 1024))
  ;; (global-evil-mc-mode 1)
  (menu-bar-mode -1)

  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls) (setq insert-directory-program gls)))

  (require 'read-aloud)
  (setq read-aloud-engine "say")

  ;;Issue: flycheck syntax checking makes editing files really slow
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 10) ;; Set delay based on what suits you the best

  ;; remove underline
  (set-face-attribute hl-line-face nil :underline nil)
  ;; Preserve syntax highlight when highlighting current line
  (set-face-foreground 'highlight nil)
  ;; Change visual selection's colors
  (set-face-attribute 'region nil :background "#cbe587" :foreground "#000000")

  (require 'font-lock+)
  (use-package all-the-icons-ivy
    :ensure t
    :config
    (progn
      ;; (setq all-the-icons-ivy-file-commands
      ;;       '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
      (all-the-icons-ivy-setup)))
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

  (use-package all-the-icons
    :ensure t
    :config
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(enh-ruby-mode all-the-icons-octicon "ruby" :face all-the-icons-lred)))


  ;; vim word with underscore
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (global-evil-matchit-mode 1)

  ;; ivy and counsel
  ;; (setq counsel-async-filter-update-time 100000)
  ;; (setq projectile-enable-caching t)

  ;; company
  (setq company-idle-delay 0.1)

  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-fuzzy)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "TAGS")
  (add-to-list 'ivy-ignore-buffers "\\*scratch\\*")

  (progn
    (set-face-attribute
     'diff-hl-insert nil :background nil :foreground "green")
    (set-face-attribute
     'diff-hl-delete nil :background nil :foreground "red")
    (set-face-attribute
     'diff-hl-change nil :background nil :foreground "blue"))

  (with-eval-after-load  'diff-hl
    (setq diff-hl-side 'right)
    (add-hook 'prog-mode-hook 'diff-hl-mode)
    (add-hook 'prog-mode-hook 'diff-hl-margin-mode)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

  (autoload 'apib-mode "apib-mode"
    "Major mode for 'editing API Blueprint files" t)
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

  (indent-guide-global-mode)

  ;; google-translate
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi")

  ;; magit
  (setq git-commit-summary-max-length 50)
  ;; docker
  (evil-leader/set-key "D b" 'dockerfile-build-buffer)

  ;; ruby
  (add-hook 'ruby-mode-hook #'rubocopfmt-mode)

  ;; ess
  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-toggle-underscore nil)))

  ;; clojure
  ;; (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel [:dev :test :cards]))")
  (setq cider-cljs-lein-repl "(do (use 'user) (start))")
  (setq clojure-indent-style :align-arguments))

(defun user-config-layout ()
  "Configuraiton for custom layout"
  )

(defun user-config-mappings ()
  "Configurations for custom mappings"

  (defun osx-active-iterm-2 ()
    "Active Iterm 2"
    (interactive)
    (shell-command "osascript -e 'activate application \"iterm 2\"'"))
  (define-key evil-normal-state-map (kbd "C-SPC 1") 'osx-active-iterm-2)

  (evil-leader/set-key-for-mode 'sql-mode "=" 'format-sql-region)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (evil-leader/set-key "a C" 'calendar)
  (evil-leader/set-key "x s" 'read-aloud-this)
  (evil-leader/set-key "w |" 'split-window-right)
  (evil-leader/set-key "e f" 'rubocop-autocorrect-current-file)
  ;; (evil-leader/set-key "'" 'hero/smart-shell)

  (defun rake-db-migrate ()
    "Rake db migrate"
    (interactive)
    (let ((command "rake db:migrate"))
      (if (projectile-project-p)
          (projectile-with-default-dir (projectile-project-root)
            (shell-command command))
        (async-shell-command command))))
  (evil-leader/set-key-for-mode 'enh-ruby-mode "r d m" 'rake-db-migrate)
  (evil-leader/set-key-for-mode 'enh-ruby-mode "g g" 'dumb-jump-go)
  (evil-leader/set-key-for-mode 'enh-ruby-mode "g G" 'dumb-jump-go-other-window)
  (evil-leader/set-key-for-mode 'ruby-mode "g g" 'dumb-jump-go)
  (evil-leader/set-key-for-mode 'ruby-mode "g G" 'dumb-jump-go-other-window)
  (evil-leader/set-key-for-mode 'haml-mode "g g" 'dumb-jump-go)
  (evil-leader/set-key-for-mode 'haml-mode "g G" 'dumb-jump-go-other-window)
  (evil-leader/set-key-for-mode 'js2-mode "g g" 'dumb-jump-go)
  (evil-leader/set-key-for-mode 'js2-mode "g G" 'dumb-jump-go-other-window)
  (evil-leader/set-key-for-mode 'rjsx-mode "g g" 'dumb-jump-go)
  (evil-leader/set-key-for-mode 'rjsx-mode "g G" 'dumb-jump-go-other-window)
  (define-key evil-normal-state-map "gs" 'save-buffer)
  (define-key evil-normal-state-map "gl" 'spacemacs/layouts-transient-state/body)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file))

(defun user-config-js ()
  "Configurations for js"
  ;; javascript
  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   js-indent-level 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-hook 'js2-init-hook
            '(lambda ()
               (setq next-error-function 'flycheck-next-error)
               ))
  (setq js-indent-align-list-continuation nil)

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  ;; (add-hook 'web-mode-hook 'prettier-js-mode)
)

(defun user-config-search-engine ()
  "Config search engine"
  ;; search engine
  (push '(oxford-dictionary
          :name "Oxford Dictionary"
          :url  "http://www.oxfordlearnersdictionaries.com/definition/english/%s")
        search-engine-alist)

  (push '(vdict-dictionary
          :name "Vdict Dictionary"
          :url  "https://vdict.com/%s,1,0,0.html")
        search-engine-alist)

  (push '(docs-lodash
          :name "Docs Lodash"
          :url  "https://lodash.com/docs/4.17.10#%s")
        search-engine-alist)


  (push '(docs-ruby
          :name "Docs Ruby"
          :url  "https://ruby-doc.org/core-2.2.0/%s.html")
        search-engine-alist)

  (defengine search-workarround "") ;; work arround to define search-oxford-dictionary

  (evil-leader/set-key "s w d l" 'engine/search-docs-lodash)
  (evil-leader/set-key "s w d r" 'engine/search-docs-ruby)
  (evil-leader/set-key "s w o" 'engine/search-oxford-dictionary)
  (evil-leader/set-key "s w v" 'engine/search-vdict-dictionary)
  (evil-leader/set-key "s w g" 'engine/search-google)
  (evil-leader/set-key "s w i" 'engine/search-google-images)
  (evil-leader/set-key "s w m" 'engine/search-google-maps))

(defun user-config-shell ()
  ""
  (defun smart-shell-pop ()
    "Invoke a shell with smart directory"
    (interactive)
    (if (projectile-project-p)
        (spacemacs/projectile-shell-pop)
      (spacemacs/default-pop-shell)))

  (evil-leader/set-key "'" 'smart-shell-pop)

  ;; ;; eterm
  ;; (with-eval-after-load  'eterm-256color
  ;;   (add-hook 'term-mode-hook #'eterm-256color-mode))
  (require 'eterm-256color)
  (add-hook 'term-mode-hook #'eterm-256color-mode)

  ;; (add-to-list 'term-bind-key-alist '("C-c C-n" . multi-term-next))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-p" . multi-term-prev))

  (defun evil-term-line-mode ()
    (interactive)
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (term-line-mode)
    (evil-normal-state))

  (defun evil-term-char-mode ()
    (interactive)
    (setq evil-insert-state-cursor '(box "yellow"))
    (term-char-mode)
    (evil-insert-state))

  (defun evil-term-return ()
    (interactive)
    (evil-term-char-mode)
    (term-send-input))


  (add-hook 'term-mode-hook
            '(lambda ()
               ;; (setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y"))
               (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
               (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))

               ;; From char mode
               (evil-declare-key 'insert term-raw-map (kbd "<escape>") 'evil-term-line-mode)
               ;; (evil-declare-key 'insert term-raw-map (kbd "RET") 'evil-term-return)
               ;; (evil-declare-key 'insert term-raw-map (kbd "<return>") 'evil-term-return)

               ;; From line mode
               (evil-declare-key 'normal term-mode-map (kbd "i") 'evil-term-char-mode)
               (evil-declare-key 'normal term-mode-map (kbd "a") 'evil-term-char-mode)
               (evil-declare-key 'normal term-mode-map (kbd "o") 'evil-term-char-mode)
               (evil-declare-key 'normal term-mode-map (kbd "a") 'evil-term-char-mode)
               (evil-declare-key 'normal term-mode-map (kbd "cw") 'evil-term-char-mode)
               (evil-declare-key 'normal term-mode-map (kbd "cc") 'evil-term-char-mode)

               (evil-declare-key 'insert term-mode-map (kbd "RET") 'evil-term-return)
               (evil-declare-key 'insert term-mode-map (kbd "<return>") 'evil-term-return)
               (evil-declare-key 'normal term-mode-map (kbd "RET") 'evil-term-return)
               (evil-declare-key 'normal term-mode-map (kbd "return") 'evil-term-return)
               ))

  ;; shell
  (setq comint-input-ring-size 1000)
  (add-hook 'shell-mode-hook 'my-shell-mode-hook)
  (defun my-shell-mode-hook ()
    (setq comint-input-ring-file-name "~/.zsh_history")  ;; or bash_history
    (setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
    (comint-read-input-ring t))

  (add-hook 'shell-mode-hook
            '(lambda ()
               (evil-declare-key 'insert shell-mode-map (kbd "C-r") 'counsel-shell-history)
               (evil-declare-key 'normal shell-mode-map (kbd "C-k") 'evil-window-up)
               (evil-declare-key 'normal shell-mode-map (kbd "C-j") 'evil-window-down))))

(defun user-config-org ()
  "Config"
  (with-eval-after-load 'org
    (evil-leader/set-key-for-mode 'org-mode "l" 'org-drill)
    (evil-leader/set-key "x a t" 'org-align-all-tags)
    (add-hook 'evil-org-mode-hook (lambda ()
                                    (evil-define-key 'normal evil-org-mode-map "-" 'org-cycle-list-bullet)))
    (add-hook 'org-mode-hook (lambda ()
                               (display-line-numbers-mode -1)))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (dot . t)
       (sql . t)
       ;; (ruby . t)
       (org . t)
       (shell . t)
       (C . t)
       (js . t)))

    ;; (require 'org-agenda)
    ;; (org-agenda-to-appt)
    (appt-activate 1)
    (setq appt-message-warning-time 10)
    (setq appt-display-interval 2)
    (setq appt-display-format 'window)
    (display-time)
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
    (defun rgr/org-display (min-to-app new-time msg)
      (shell-command
       (concat "terminal-notifier "
               "-title 'Reminder' "
               "-message '" msg "' "
               "-sound default "
               ;; "-appIcon ~/org-modes/bot.jpg"
               )))
    (setq appt-disp-window-function (function rgr/org-display))

    ;; org-capture
    (setq org-capture-templates
          '(
            ("v"
             "Vocabulary"
             entry
             (file "~/org-modes/flashcards.org")
             "* %i%^{prompt} :drill: \n\nTranslate\n\n** Answer\n\n[[file:~/org-modes/images/%\\1.png]]\n\n** Image Source\n\n#+begin_src shell\ntest -f ~/org-modes/images/%\\1.png || wget -O ~/org-modes/images/%\\1.png \"%\\1%?\"\n#+end_src")
            ("s"
             "Speaking English"
             entry
             (file "~/org-modes/flashcards.org")
             "* %i%^{sentence} :drill:speaking:\n:PROPERTIES:\n:DRILL_SOUND: ~/org-modes/english/%^{sound}\n:END:\n\nSpeaking loudly man\n\n** Answer\n\n[[file:~/org-modes/english/%\\2]]\n\n** Source\n\n#+begin_src shell\ntest -f ~/org-modes/english/%\\2 || wget -O ~/org-modes/english/%\\2 \"%^{link}\"\n#+end_src")
            ("a"
             "Appointment"
             entry
             (file+headline "~/org-modes/personal.org" "Appointments")
             "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n"
             )
            ("l"
             "learn"
             entry
             (file "~/org-modes/learn.org")
             "* %? :drill:\n"
             )
            ("c"
             "Reading List"
             entry
             (file+headline "~/org-modes/personal.org" "Reading List")
             "* TODO %?\n")
            ("p"
             "Article"
             entry
             (file+headline "~/org-modes/articles.org" "Articles")
             "* %:link\n")))

    (require 'org-checklist)
    ;; org-drill
    (setq org-drill-maximum-items-per-session 40)
    (setq org-drill-maximum-duration 30)   ; 30 minutes
    (setq org-drill-learn-fraction 0.1)
    (setq org-drill-spaced-repetition-algorithm 'sm2)

    ;; customize
    (with-eval-after-load 'org-drill
      (defun org-drill-sound ()
        (interactive)
        (if-let ((drill-sound (org-entry-get (point) "DRILL_SOUND")))
            (start-process-shell-command "mplayer" "*sound*" "mplayer"
                                         (expand-file-name drill-sound))
          (read-aloud--string (org-get-heading t t t t) "word")))
      (evil-leader/set-key-for-mode 'org-mode "r" 'org-drill-sound)
      (defvar org-drill--repeat-key ?r "")
      (defun org-drill-presentation-prompt (&rest fmt-and-args)
        (let* ((item-start-time (current-time))
               (input nil)
               (ch nil)
               (last-second 0)
               (mature-entry-count (+ (length *org-drill-young-mature-entries*)
                                      (length *org-drill-old-mature-entries*)
                                      (length *org-drill-overdue-entries*)))
               (status (first (org-drill-entry-status)))
               (prompt
                (if fmt-and-args
                    (apply 'format
                           (first fmt-and-args)
                           (rest fmt-and-args))
                  (format (concat "Press key for answer, "
                                  "%c=edit, %c=tags, %c=skip, %c=repeat, %c=quit.")
                          org-drill--edit-key
                          org-drill--tags-key
                          org-drill--skip-key
                          org-drill--repeat-key
                          org-drill--quit-key))))
          (setq prompt
                (format "%s %s %s %s %s %s"
                        (propertize
                         (char-to-string
                          (cond
                           ((eql status :failed) ?F)
                           (*org-drill-cram-mode* ?C)
                           (t
                            (case status
                              (:new ?N) (:young ?Y) (:old ?o) (:overdue ?!)
                              (t ??)))))
                         'face `(:foreground
                                 ,(case status
                                    (:new org-drill-new-count-color)
                                    ((:young :old) org-drill-mature-count-color)
                                    ((:overdue :failed) org-drill-failed-count-color)
                                    (t org-drill-done-count-color))))
                        (propertize
                         (number-to-string (length *org-drill-done-entries*))
                         'face `(:foreground ,org-drill-done-count-color)
                         'help-echo "The number of items you have reviewed this session.")
                        (propertize
                         (number-to-string (+ (length *org-drill-again-entries*)
                                              (length *org-drill-failed-entries*)))
                         'face `(:foreground ,org-drill-failed-count-color)
                         'help-echo (concat "The number of items that you failed, "
                                            "and need to review again."))
                        (propertize
                         (number-to-string mature-entry-count)
                         'face `(:foreground ,org-drill-mature-count-color)
                         'help-echo "The number of old items due for review.")
                        (propertize
                         (number-to-string (length *org-drill-new-entries*))
                         'face `(:foreground ,org-drill-new-count-color)
                         'help-echo (concat "The number of new items that you "
                                            "have never reviewed."))
                        prompt))
          (org-drill-sound)
          (if (and (eql 'warn org-drill-leech-method)
                   (org-drill-entry-leech-p))
              (setq prompt (concat
                            (propertize "!!! LEECH ITEM !!!
  You seem to be having a lot of trouble memorising this item.
  Consider reformulating the item to make it easier to remember.\n"
                                        'face '(:foreground "red"))
                            prompt)))
          (while (memq ch '(nil org-drill--tags-key))
            (setq ch nil)
            (while (not (input-pending-p))
              (let ((elapsed (time-subtract (current-time) item-start-time)))
                (message (concat (if (>= (time-to-seconds elapsed) (* 60 60))
                                     "++:++ "
                                   (format-time-string "%M:%S " elapsed))
                                 prompt))
                (sit-for 1)))
            (setq input (read-key-sequence nil))
            (if (stringp input) (setq ch (elt input 0)))
            (if (eql ch org-drill--tags-key)
                (org-set-tags-command)))
          (case ch
            (org-drill--quit-key nil)
            (org-drill--edit-key 'edit)
            (org-drill--skip-key 'skip)
            (org-drill--repeat-key 'sound)
            (otherwise t))))
      (defun org-drill-reschedule ()
        "Returns quality rating (0-5), or nil if the user quit."
        (let ((ch nil)
              (input nil)
              (next-review-dates (org-drill-hypothetical-next-review-dates))
              (key-prompt (format "(0-5, %c=help, %c=edit, %c=tags, %c=repeat, %c=quit)"
                                  org-drill--help-key
                                  org-drill--edit-key
                                  org-drill--tags-key
                                  org-drill--repeat-key
                                  org-drill--quit-key)))
          (save-excursion
            (while (not (memq ch (list org-drill--quit-key
                                       org-drill--edit-key
                                       7          ; C-g
                                       ?0 ?1 ?2 ?3 ?4 ?5)))
              (setq input (read-key-sequence
                           (if (eq ch org-drill--help-key)
                               (format "0-2 Means you have forgotten the item.
  3-5 Means you have remembered the item.

  0 - Completely forgot.
  1 - Even after seeing the answer, it still took a bit to sink in.
  2 - After seeing the answer, you remembered it.
  3 - It took you awhile, but you finally remembered. (+%s days)
  4 - After a little bit of thought you remembered. (+%s days)
  5 - You remembered the item really easily. (+%s days)

  How well did you do? %s"
                                       (round (nth 3 next-review-dates))
                                       (round (nth 4 next-review-dates))
                                       (round (nth 5 next-review-dates))
                                       key-prompt)
                             (format "How well did you do? %s" key-prompt))))
              (cond
               ((stringp input)
                (setq ch (elt input 0)))
               ((and (vectorp input) (symbolp (elt input 0)))
                (case (elt input 0)
                  (up (ignore-errors (forward-line -1)))
                  (down (ignore-errors (forward-line 1)))
                  (left (ignore-errors (backward-char)))
                  (right (ignore-errors (forward-char)))
                  (prior (ignore-errors (scroll-down))) ; pgup
                  (next (ignore-errors (scroll-up)))))  ; pgdn
               ((and (vectorp input) (listp (elt input 0))
                     (eventp (elt input 0)))
                (case (car (elt input 0))
                  (wheel-up (ignore-errors (mwheel-scroll (elt input 0))))
                  (wheel-down (ignore-errors (mwheel-scroll (elt input 0)))))))
              (if (eql ch org-drill--tags-key)
                  (org-set-tags-command))
              (if (eql ch org-drill--repeat-key)
                  (org-drill-sound))
              ))
          (cond
           ((and (>= ch ?0) (<= ch ?5))
            (let ((quality (- ch ?0))
                  (failures (org-drill-entry-failure-count)))
              (unless *org-drill-cram-mode*
                (save-excursion
                  (let ((quality (if (org-drill--entry-lapsed-p) 2 quality)))
                    (org-drill-smart-reschedule quality
                                                (nth quality next-review-dates))))
                (push quality *org-drill-session-qualities*)
                (cond
                 ((<= quality org-drill-failure-quality)
                  (when org-drill-leech-failure-threshold
                    ;;(setq failures (if failures (string-to-number failures) 0))
                    ;; (org-set-property "DRILL_FAILURE_COUNT"
                    ;;                   (format "%d" (1+ failures)))
                    (if (> (1+ failures) org-drill-leech-failure-threshold)
                        (org-toggle-tag "leech" 'on))))
                 (t
                  (let ((scheduled-time (org-get-scheduled-time (point))))
                    (when scheduled-time
                      (message "Next review in %d days"
                               (- (time-to-days scheduled-time)
                                  (time-to-days (current-time))))
                      (sit-for 0.5)))))
                (org-set-property "DRILL_LAST_QUALITY" (format "%d" quality))
                (org-set-property "DRILL_LAST_REVIEWED"
                                  (time-to-inactive-org-timestamp (current-time))))
              quality))
           ((= ch org-drill--edit-key)
            'edit)
           (t
            nil))))
      )
    )

  (setq org-confirm-babel-evaluate nil)

  ;; uml
  (setq org-plantuml-jar-path "~/org-modes/plantuml.jar"))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (if (display-graphic-p) (user-config-gui) (user-config-tui))

  (user-config-general)
  (user-config-search-engine)
  (user-config-org)
  (user-config-js)
  (user-config-shell)
  (user-config-layout)
  (user-config-mappings))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
