;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
(setq doom-theme 'doom-one)

(setq doom-localleader-key ",")

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org-modes")

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

(after! ivy (load! "ivy"))

(when (file-exists-p (concat doom-private-dir "private"))
    (load! "private/+bindings")
    (load! "private/prodigy")
    (load! "private/hero"))
