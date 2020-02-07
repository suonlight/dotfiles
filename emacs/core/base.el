;;; -*- lexical-binding: t; -*-
;; (setq inhibit-startup-screen t)
;; (setq initial-scratch-message ";; Happy Hacking")

;; emacs 27
;; (set-frame-font "-ADBE-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(set-face-font 'default "-ADBE-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(setq default-frame-alist '((font ."-ADBE-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))) ;;; set default font for emacs --daemon / emacsclient

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(setq-default truncate-lines nil)
(setq-default global-visual-line-mode t)
(setq make-backup-files nil)
;; (toggle-frame-fullscreen)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq tags-add-tables nil)

(setq mode-require-final-newline t)
(setq require-final-newline t)

;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'none)

;; (setq mac-command-modifier 'meta
;;       mac-option-modifier 'none)

(setq use-package-verbose t)
