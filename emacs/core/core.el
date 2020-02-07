;;; -*- lexical-binding: t; -*-

(setq display-line-numbers-type 'relative)

(use-package which-key
  :defer .1
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config (which-key-mode))

(use-package evil
  ;; :defer .1
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-search-module 'evil-search)
  :config
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))

  (define-key evil-inner-text-objects-map "g" #'evil-inner-buffer)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package evil-commentary
  :after evil
  :bind (:map evil-normal-state-map
	      ("gc" . evil-commentary)))

(use-package evil-surround
  :after evil
  ;; :commands (evil-surround-edit evil-Surround-edit evil-surround-region evil-Surround-region)
  :init
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region)
  :config (global-evil-surround-mode))

(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

(use-package evil-goggles
  :config
  (setq evil-goggles-duration 0.100)
  (setq evil-goggles-enable-paste nil)
  (setq evil-goggles-enable-change nil)
  (setq evil-goggles-enable-commentary nil)
  (evil-goggles-mode))

(use-package evil-string-inflection :after evil :commands evil-operator-string-inflection)

(use-package avy
  :commands (avy-pop-mark avy-resume evil-avy-goto-char-timer evil-avy-goto-word-or-subword-1 evil-avy-goto-char-2)
  :config (avy-setup-default))

(use-package dumb-jump
  :commands (dumb-jump-go-other-window dumb-jump-go dumb-jump-back dumb-jump-go-prefer-external dumb-jump-go-prefer-external-other-window)
  :config
  (setq dumb-jump-default-project "~/projects")
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode nil)
  :config
  (setq flycheck-check-syntax-automatically '(save)))

(setq-default flycheck-disabled-checkers '(ruby-reek emacs-lisp emacs-lisp-checkdoc javascript-jshint))

(use-package indent-guide
  :commands (indent-guide-mode))

(use-package iedit :bind ("C-;" . iedit-mode))

(use-package evil-iedit-state
  :after iedit
  :bind ("C-;" . evil-iedit-state/iedit-mode))


(use-package yasnippet :hook (after-init . yas-global-mode))

(use-package smartparens
  :hook (after-init . smartparens-global-mode))

(use-package editorconfig
  :delight editorconfig-mode
  :hook (after-init . editorconfig-mode))

(use-package drag-stuff
  :commands (drag-stuff-up drag-stuff-down)
  :config (drag-stuff-global-mode 1))

(use-package move-text
  :commands (move-text-line-up move-text-line-down))

;; window
(use-package winum
  :defer t
  :config
  (winum-mode))

(use-package winner
  :commands (winner-undo window-redo)
  :config (winner-mode))

(use-package request
  :commands request)

(load (concat user-emacs-directory "core/binding"))
(load (concat user-emacs-directory "core/core-lib"))
(load (concat user-emacs-directory "core/core-projects"))
(load (concat user-emacs-directory "core/core-git"))
(load (concat user-emacs-directory "core/core-ui"))
(load (concat user-emacs-directory "core/workspace"))
(load (concat user-emacs-directory "core/terminal"))
(load (concat user-emacs-directory "core/lang-ruby"))
(load (concat user-emacs-directory "core/lang-rust"))
(load (concat user-emacs-directory "core/lang-js"))
(load (concat user-emacs-directory "core/lang-others"))
(load (concat user-emacs-directory "core/org-mode"))
(load (concat user-emacs-directory "core/auto-complete"))
(load (concat user-emacs-directory "core/debugger"))
(load (concat user-emacs-directory "core/lsp"))
(load (concat user-emacs-directory "core/tools"))
