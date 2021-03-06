;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; (setq debug-on-error t)
(setq debug-on-error nil)

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Minh Nguyen Hue"
  user-mail-address "minh.nh1989@gmail.com")

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-idle-delay 0.500)
(setq lsp-prefer-capf t)
(setq auth-sources '("~/.authinfo"))

;; (setq scroll-step           1
;;   scroll-conservatively 10000)

(setq scroll-margin 0
  scroll-conservatively 10000
  scroll-up-aggressively 0.03
  scroll-down-aggressively 0.03)

(setq straight-disable-native-compilation t)

(setq comp-speed 3
  comp-deferred-compilation t)
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
(setq doom-font (font-spec :family "Source Code Pro" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq doom-theme 'doom-dark+)
(setq doom-themes-treemacs-theme "doom-colors")

(setq doom-localleader-key ",")

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Dropbox/org-modes")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-type nil)

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(setq mode-require-final-newline t)
(setq require-final-newline t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(load! "+bindings")

;; prevent eslint check command: eslint --print-config .
(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

(when (file-exists-p (concat doom-private-dir "private"))
  (load! "private/+bindings")
  (load! "private/prodigy")
  (load! "private/hero")
  (load! "private/js-import")
  (load! "private/config"))

(load! "utils")
(load! "ivy")
(load! "org-mode")

(setq avy-all-windows t)

;; (after! treemacs (doom-themes-treemacs-config))

(after! company
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0.3))

(after! lsp
  (setq rustic-lsp-server 'rust-analyzer) ;; it's not ready yet
  (setq lsp-auto-guess-root nil))

(after! flycheck
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-indication-mode nil)
  (setq flycheck-check-syntax-automatically '(save))
  (setq-default flycheck-disabled-checkers '(ruby-reek emacs-lisp emacs-lisp-checkdoc javascript-jshint)))

(after! evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(after! ruby-mode
  (set-company-backend! 'ruby-mode '(company-capf company-abbrev company-dabbrev-code company-files company-etags company-keywords company-yasnippet)))

(after! rspec-mode
  (setq rspec-use-bundler-when-possible nil))

;; (add-hook! ruby-mode (add-hook 'before-save-hook #'lsp-format-buffer t t))

(after! js
  (set-company-backend! 'js-mode '(company-capf company-dabbrev-code company-files company-yasnippet)))

(after! projectile
  (setq projectile-tags-file-name "ETAGS"))

;; (after! counsel-etags
;;   (setq counsel-etags-tags-file-name "ETAGS"))

(after! vterm
  (defun auto-swith-to-insert ()
    "Go back to normal state but don't move
cursor backwards. Moving cursor backwards is the default vim behavior but it is
not appropriate in some cases like terminals."
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state)
    (setq-local evil-move-cursor-back nil))

  (add-hook! vterm-mode #'auto-swith-to-insert))

; (after! git-messenger
;   (setq git-messenger:use-magit-popup t))

(after! magit
  ;; (setq magit-git-executable "/usr/bin/git")

  ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  (remove-hook 'magit-status-sections-hook #'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook #'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent))

(after! forge
  ;; (add-hook! forge-post-mode #'sl/make-draft-pr)

  (defun forge--display-post-buffer (buf)
    (magit-display-buffer buf #'display-buffer)
    (sl/make-draft-pr))

  (setq forge-topic-list-limit '(0 . 0)))

(set' +zen-text-scale 3)

(setq ispell-dictionary "en")
(setq ispell-personal-dictionary "~/projects/doom-emacs/.local/etc/ispell/en.pws")

;; ReasonML
(use-package! reason-mode
  :commands reason-mode
  :config
  (add-hook! reason-mode #'lsp))

(after! lsp-mode
  (setq lsp-response-timeout 20)
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "~/.config/doom/assets/rls-macos/reason-language-server")
      :major-modes '(reason-mode)
      :notification-handlers (ht ("client/registerCapability" 'ignore))
      :priority 1
      :server-id 'reason-ls)))

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

(set-popup-rule! "^\\*Process List\\*" :select t :size 0.35)
(set-popup-rule! "^\\*prodigy\\*" :select t :size 0.35)
(set-popup-rule! "^\\*rspec-compilation\\*" :select t :size 0.35)
(set-popup-rule! "^\\*vterminal" :ignore t :select t :size 0.35)
(set-popup-rule! "^\\*VC-history*" :select t :size 0.5)

;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

(after! plantuml-mode
  (setq plantuml-jar-path "~/org-modes/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package! evil-string-inflection
  :after evil
  :commands evil-operator-string-inflection)

(use-package! request :commands request)

(use-package! multi-vterm
  :commands (multi-vterm multi-vterm-project)
  :config
  (defun my/project-find-dot-project (dir)
    (when-let ((root (locate-dominating-file dir ".project")))
      `(dot-project . ,root)))

  (cl-defmethod project-roots ((project (head dot-project)))
    (list (cdr project)))

  (push #'my/project-find-dot-project project-find-functions))

(use-package! prettier-js
  :config
  (add-hook! js-mode prettier-js-mode)
  (add-hook! web-mode prettier-js-mode))

(defun google-translate-at-point ()
  (interactive)
  (require 'google-translate)
  (google-translate-at-point))

(use-package! google-translate
  :commands (google-translate-at-point)
  :init
  (setq google-translate-show-phonetic t)
  (setq google-translate-backend-method 'wget)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "vi")
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package! evil-matchit
  :config
  (add-hook! js-mode evil-matchit-mode)
  (add-hook! ruby-mode evil-matchit-mode))

(use-package! indent-guide
  :commands indent-guide-global-mode)

(use-package! nov-mode
  :mode "\\.epub$")

(use-package! protobuf-mode
  :mode "\\.proto$")

(after! dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package! read-aloud
  :commands (read-aloud-this read-aloud-buf read-aloud--string)
  :config (setq read-aloud-engine "say"))

;; (use-package! grammarly)
;; (use-package! flycheck-grammarly
;;   :config (setq flycheck-grammarly-check-time 0.8))

; (use-package! git-link
;   :commands (git-link git-link-commit)
;   :config
;   (setq git-link-open-in-browser t))

;; (use-package! tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode))

(when IS-LINUX
  (after! auth-source
    (setq auth-sources '("~/.authinfo")))

  (defun sl/async-run-command (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun sl/exwm-config-custom ()
    "Default configuration of EXWM."
    ;; Set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 2))

    (setq exwm-input-prefix-keys
      '(?\s-x
         ?\C-x
         ?\C-l
         ?\C-h
         ?\C-j
         ?\C-k
         ?\C-c
         ?\s-b
         ?\s-w
         ?\s-t
         ?\s-`
         ?\s-1
         ?\s-2
         ?\s-3
         ?\s-4
         ?\s-5
         ?\s-6
         ?\s-7
         ?\s-8
         ?\s-9
         ?\s-0
         ?\C-\ ))

    ;; Global keybindings.
    (unless (get 'exwm-input-global-keys 'saved-value)
      (setq exwm-input-global-keys
        `(
           ;; 's-r': Reset (to line-mode).
           ([?\s-r] . exwm-reset)
           ;; 's-w': Switch workspace.
           ;; ([?\s-w] . exwm-workspace-switch)
           ;; 's-&': Launch application.
           ([?\s-&] . (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

           ;; ([?\s-`] . (lambda ()
           ;;              (interactive)
           ;;              (exwm-workspace-switch-create 0)))

           ;; 's-N': Switch to certain workspace.
           ;; ,@(mapcar (lambda (i)
           ;;             `(,(kbd (format "s-%d" i)) .
           ;;                (lambda ()
           ;;                  (interactive)
           ;;                  (exwm-workspace-switch-create ,i))))
           ;;     (number-sequence 0 9))
           )))

    ;; Line-editing shortcuts
    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
           ([?\C-f] . [right])
           ([?\C-p] . [up])
           ([?\C-n] . [down])
           ([?\C-a] . [home])
           ([?\C-e] . [end])
           ([?\M-v] . [prior])
           ([?\C-v] . [next])
           ([?\C-d] . [delete])
           ([?\s-v] . [S-insert])
           ([?\s-c] . [C-c])
           ([?\C-k] . [S-end delete]))))

    (exwm-input-set-key (kbd "s-SPC") #'counsel-linux-app)
    ;; Enable EXWM
    (exwm-enable)
    )

  (defun sl/exwm-init-hook ()
    (exwm-workspace-switch-create 0)
    (menu-bar-mode -1)

    ;; start polybar panel
    (sl/start-panel)

    ;; (sl/async-run-command "pasystray")
    (sl/async-run-command "ibus-daemon --xim"))

  (defun sl/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (defun sl/exwm-update-title ()
    (pcase exwm-class-name
      ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

  (defun sl/configure-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ("Slack" (exwm-layout-toggle-mode-line))))

  (defvar sl/polybar-process nil
    "Holds the process of the running Polybar instance, if any")

  (defun sl/kill-panel ()
    (interactive)
    (when sl/polybar-process
      (ignore-errors
        (kill-process sl/polybar-process)))
    (setq sl/polybar-process nil))

  (defun sl/start-panel ()
    (interactive)
    (sl/kill-panel)
    (setq sl/polybar-process (start-process-shell-command "polybar" nil "polybar -q main -c ~/.config/polybar/config.ini")))

  (defun sl/send-polybar-hook (module-name hook-index)
    (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

  (defun sl/send-polybar-exwm-workspace ()
    (sl/send-polybar-hook "exwm-workspaces" 1))

  (defvar sl/current-buffer nil)

  (defun sl/buffer-icon ()
    (cond
      ((string= major-mode "org-mode") "")
      ((string= major-mode "vterm-mode") "")
      ((string= major-mode "ruby-mode") "")
      ((string= major-mode "js-mode") "")
      ((or (string= major-mode "docker-mode") (string-match-p "Dockerfile" (buffer-name))) "")
      ((string-match-p "Firefox" (buffer-name)) "")
      ((string-match-p "Slack" (buffer-name)) "")
      (t "")))

  (defun sl/send-polybar-emacs-modeline ()
    (setq sl/current-buffer (if buffer-file-truename
                              (format "%s %s %d:%d ⏽ %s %s"
                                (sl/buffer-icon)
                                buffer-file-truename
                                (current-column)
                                (line-number-at-pos)
                                (pcase (coding-system-eol-type buffer-file-coding-system)
                                  (0 "LF")
                                  (1 "CRLF")
                                  (2 "CR"))
                                (coding-system-type buffer-file-coding-system))
                              (format "%s %s" (sl/buffer-icon)
                                (truncate-string-to-width (buffer-name) 100 nil nil t))))
    (sl/send-polybar-hook "emacs-modeline" 1))

  (defun sl/polybar-current-buffer ()
    sl/current-buffer)

  (defun sl/polybar-exwm-workspace ()
    (pcase exwm-workspace-current-index
      (0 "")
      (1 "")
      (2 "")
      (3 "")
      (4 "")))

  (setq-default mode-line-format nil)
  ;; (setq-default doom-modeline-mode nil)

  (use-package! exwm
    :config

    (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)
    (add-hook 'exwm-init-hook #'sl/exwm-init-hook)
    (add-hook 'exwm-update-class-hook #'sl/exwm-update-class)
    (add-hook 'exwm-update-title-hook #'sl/exwm-update-title)
    (add-hook 'exwm-manage-finish-hook #'sl/configure-window-by-class)
    (add-hook 'exwm-workspace-switch-hook #'sl/send-polybar-exwm-workspace)
    (add-hook 'post-command-hook #'sl/send-polybar-emacs-modeline)

    ;; (start-process-shell-command "xrandr" nil "")
    ;; (add-hook 'isearch-update-post-hook 'redraw-display)
    ;; (advice-add 'select-window :around #'sl/send-polybar-emacs-modeline)
    ;; (advice-remove 'select-window #'sl/send-polybar-emacs-modeline)

    ;; ;; load system tray before exwm-init
    ;; (require 'exwm-systemtray)
    ;; (setq exwm-systemtray-height 16)
    ;; (exwm-systemtray-enable)

    (sl/exwm-config-custom))

  (setq run-command-recipes
    (list
      (list :display "Lauch Firefox" :command-line "firefox")
      (list :display "Launch App Setting" :command-line "gnome-control-center")
      (list :display "Start Postgres Server" :command-line "pg_ctl start")))

  (defun run-command ()
    (interactive)
    (let ((recipes (mapcar
                     (lambda (x)
                       (propertize (plist-get x :display) 'property (plist-get x :command-line)))
                     run-command-recipes)))
      (ivy-read "Run command: " recipes
        :action (lambda (recipe)
                  (let ((command (get-text-property 0 'property recipe)))
                    (start-process-shell-command command nil command)))))))

(setq default-input-method "vietnamese-telex")

(global-set-key (kbd "C-x 2")  #'sl/split-below-last-buffer)
(global-set-key (kbd "C-x 3")  #'sl/split-right-last-buffer)
(setq switch-to-prev-buffer-skip 'this)
