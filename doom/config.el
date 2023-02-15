;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Minh Nguyen Hue"
  user-mail-address "minh.nh1989@gmail.com")

;; (setq debug-on-error t)
;; (setq debug-on-error nil)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
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
;; (setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-themes-treemacs-theme "doom-colors")

;; (setq doom-localleader-key ",")
(keymap-set evil-motion-state-map "," (general-simulate-key "SPC m"))

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Dropbox/org-modes")
(setq bookmark-default-file "~/Dropbox/org-modes/bookmarks")
(setq projectile-known-projects-file "~/Dropbox/org-modes/projectile.projects")
(setq org-babel-python-command "python3")
(setq python-shell-interpreter "python3")

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
(setq imenu-max-item-length 120)

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

(when (file-exists-p (concat doom-private-dir "private"))
  (load! "private/+bindings")
  (load! "private/prodigy")
  (load! "private/hero")
  (load! "private/js-import")
  (load! "private/config"))

;; prevent eslint check command: eslint --print-config .
(advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

(setq avy-all-windows t)

;; (after! treemacs (doom-themes-treemacs-config))

;; (setq doom-theme 'doom-dark+)
(setq doom-theme 'doom-one)
;; (setq doom-theme 'nano-dark)
;; (setq doom-theme 'doom-solarized-light)

; (use-package! nano-modeline :init (add-hook! emacs-startup #'nano-modeline-mode))

(after! flycheck
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-indication-mode nil)
  (setq flycheck-check-syntax-automatically '(save))
  (setq-default flycheck-disabled-checkers '(ruby-reek emacs-lisp emacs-lisp-checkdoc javascript-jshint)))

(after! evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(after! ruby-mode
  ;; (set-company-backend! 'ruby-mode '(company-capf company-abbrev company-dabbrev-code company-files company-etags company-keywords company-yasnippet))
  ;; (set-company-backend! 'rjsx-mode '(:separate company-files company-capf company-yasnippet company-dabbrev-code))

  (defun select-ruby-checker () (flycheck-select-checker 'ruby-rubocop))
  (add-hook! ruby-mode #'select-ruby-checker))

(after! sql
  (add-hook! sql-mode #'format-all-mode))

(after! format-all
  (define-format-all-formatter sqlformat
    (:executable "sqlformat")
    (:install "pip install sqlparse")
    (:modes sql-mode)
    (:format
      (let* ((ic (car default-process-coding-system))
              (oc (cdr default-process-coding-system))
              (ienc (symbol-name (or (coding-system-get ic :mime-charset)
                                   'utf-8)))
              (oenc (symbol-name (or (coding-system-get oc :mime-charset)
                                   'utf-8)))
              (process-environment (cons (concat "PYTHONIOENCODING=" oenc)
                                     process-environment)))
        (format-all--buffer-easy
          executable
          "--keywords" "upper"
          "-r" ;; old --reindent_aligned
          "--encoding" ienc
          "-")))))

(after! rspec-mode
  (set-popup-rule! "^\\*rspec-compilation\\*" :select t :size 0.35)
  (setq rspec-use-bundler-when-possible t))

(after! rjsx-mode
  (defun select-js-eslint () (flycheck-select-checker 'javascript-eslint))
  (add-hook! rjsx-mode #'select-js-eslint))

;; (after! (:and typescript-mode lsp-mode)
;;   (defun add-ts-checkers () (flycheck-add-next-checker 'lsp 'javascript-eslint))

;;   (add-hook! typescript-mode #'add-ts-checkers)
;;   (add-hook! typescript-tsx-mode #'add-ts-checkers))

(after! projectile
  (setq projectile-tags-file-name "ETAGS"))

(after! vterm
  (set-popup-rule! "^\\*vterm" :ignore t :select t :size 0.35)

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
  (set-popup-rule! "^\\*VC-history*" :select t :size 0.5)

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
  ;;
  (map! :map forge-post-mode-map
    "C-c C-c" #'sl/post-draft-pull-request)

  (defun forge--display-post-buffer (buf)
    (magit-display-buffer buf #'display-buffer)
    (sl/add-pr-template))

  (setq forge-topic-list-limit '(0 . 0)))

(setq ispell-dictionary "en")
(setq ispell-personal-dictionary "~/projects/doom-emacs/.local/etc/ispell/en.pws")

;; ;; ReasonML
;; (use-package! reason-mode
;;   :commands reason-mode
;;   :config
;;   (add-hook! reason-mode #'lsp))

(after! spell-fu
  (setq spell-fu-idle-delay 0.5))

(set-popup-rule! "^\\*Process List\\*" :select t :size 0.35)
(set-popup-rule! "^\\*Async Shell Command\\*" :ttl 0 :size 1)

(after! plantuml-mode
  (setq plantuml-jar-path "~/org-modes/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

(use-package! eacl
  :commands (eacl-complete-line eacl-complete-multiline)
  :config

  ;; Force eacl using grep instead of git grep
  (defun eacl-git-p ()
    "Return non-nil if current file is in a git repository."
    nil)

  (dolist (v '("node_modules"
                "bower_components"
                ".sass_cache"
                ".cache"
                ".npm"
                "flow"
                "flow-typed"
                ".circleci"
                ".github"
                ".idea"
                ".log"
                ".translation"
                "ContractPdfPreview"
                "dist"
                "build"
                "ios"
                "android"
                "__mocks__"))
    (add-to-list 'grep-find-ignored-directories v))

  (dolist (v '("*.min.js"
                "*.bundle.js"
                "*.min.css"
                "*.json"
                "*.log"))
    (add-to-list 'grep-find-ignored-files v)))

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
  (add-hook! typescript-mode #'prettier-js-mode) ;; remember to install ts-ls by using lsp-install-server
  (add-hook! js-mode #'prettier-js-mode)
  (add-hook! web-mode #'prettier-js-mode)

  (add-hook! js-ts-mode #'prettier-js-mode)
  (add-hook! typescript-ts-mode #'prettier-js-mode)
  (add-hook! tsx-ts-mode #'prettier-js-mode))

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
  (add-hook! js-mode #'evil-matchit-mode)
  (add-hook! ruby-mode #'evil-matchit-mode))

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

;; Config email
(setq message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; (use-package! grammarly)
;; (use-package! flycheck-grammarly
;;   :config (setq flycheck-grammarly-check-time 0.8))

; (use-package! git-link
;   :commands (git-link git-link-commit)
;   :config
;   (setq git-link-open-in-browser t))

(after! smerge-mode
  (defhydra sl/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
 ^Move^       ^Keep^               ^Diff^                 ^Other^
 ^^-----------^^-------------------^^---------------------^^-------
 _n_ext       _b_ase               _<_: upper/base        _C_ombine
 _p_rev       _u_pper              _=_: upper/lower       _r_esolve
 ^^           _l_ower              _>_: base/lower        _k_ill current
 ^^           _a_ll                _R_efine
 ^^           _RET_: current       _E_diff
 "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
	    (interactive)
	    (save-buffer)
	    (bury-buffer))
      "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))

   (add-hook 'magit-diff-visit-file (lambda () (when smerge-mode (sl/smerge-hydra/body)))))

(global-set-key (kbd "C-x 2")  #'sl/split-below-last-buffer)
(global-set-key (kbd "C-x 3")  #'sl/split-right-last-buffer)
(setq switch-to-prev-buffer-skip 'this)
