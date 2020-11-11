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
(setq doom-font (font-spec :family "monospace" :size 14))

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
(global-evil-matchit-mode 1)

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

(add-hook! js-mode prettier-js-mode)
(add-hook! web-mode prettier-js-mode)

;; prevent eslint check command: eslint --print-config .
(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

(when (file-exists-p (concat doom-private-dir "private"))
  (load! "private/+bindings")
  (load! "private/prodigy")
  (load! "private/hero")
  (load! "private/config"))

(load! "utils")
(load! "ivy")
(load! "org-mode")
(load! "gcal")

(setq avy-all-windows t)

(after! treemacs
  (doom-themes-treemacs-config))

(after! company
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0.3))

(setq rustic-lsp-server 'rust-analyzer) ;; it's not ready yet
(after! lsp
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

;; (add-hook! ruby-mode (add-hook 'before-save-hook #'lsp-format-buffer t t))

(after! projectile
  (setq projectile-tags-file-name "ETAGS"))

(after! counsel-etags
  (setq counsel-etags-tags-file-name "ETAGS"))

(add-hook! vterm-mode :append
  (defun auto-swith-to-insert ()
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state)))

(after! git-link
  (setq git-link-open-in-browser t))

(after! git-messenger
  (setq git-messenger:use-magit-popup t))

(use-package! evil-string-inflection :after evil :commands evil-operator-string-inflection)
(use-package! request)
(use-package! reason-mode)

(after! magit
  (setq magit-git-executable "/usr/bin/git")
  ;; Hide "Recent Commits"
  ;; https://github.com/magit/magit/issues/3230
  (magit-add-section-hook 'magit-status-sections-hook
    'magit-insert-unpushed-to-upstream
    'magit-insert-unpushed-to-upstream-or-recent
    'replace))

(after! forge
  ;; (add-hook! forge-post-mode #'sl/make-draft-pr)
  (setq forge-topic-list-limit '(0 . 0)))

(set' +zen-text-scale 3)

;; (use-package! grammarly)
;; (use-package! flycheck-grammarly
;;   :config (setq flycheck-grammarly-check-time 0.8))

(setq google-translate-show-phonetic t)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "vi")

(setq ispell-dictionary "en")
(setq ispell-personal-dictionary "/Users/employmenthero/projects/doom-emacs/.local/etc/ispell/en.pws")

;; ReasonML
(after! reason-mode
  (add-hook! reason-mode #'lsp)
  ;; (add-hook! reason-mode (add-hook 'before-save-hook #'lsp-format-buffer nil t))
  ;; (add-hook 'reason-mode-hook (lambda ()
  ;;         (add-hook 'before-save-hook #'refmt-before-save)))
  )

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

(after! multi-vterm
  (defun my/project-find-dot-project (dir)
    (when-let ((root (locate-dominating-file dir ".project")))
      `(dot-project . ,root)))

  (cl-defmethod project-roots ((project (head dot-project)))
    (list (cdr project)))

  (push #'my/project-find-dot-project project-find-functions))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(set-popup-rule! "^\\*Process List\\*" :select t :size 0.35)
(set-popup-rule! "^\\*prodigy\\*" :select t :size 0.35)
(set-popup-rule! "^\\*rspec-compilation\\*" :select t :size 0.35)
(set-popup-rule! "^\\*vterm " :ignore t :select t :size 0.35)
(set-popup-rule! "^\\*VC-history*" :select t :size 0.5)

;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
