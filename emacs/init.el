;;; -*- lexical-binding: t; -*-

;; Package configs
(require 'package)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")))

(defvar bootstrap-version)
(let ((bootstrap-file

       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package benchmark-init :config (add-hook 'after-init-hook #'benchmark-init/deactivate))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (setq gc-cons-threshold 100000000
		gc-cons-percentage 0.1))

;; (byte-recompile-directory "~/.emacs.d/core" 0 t)
(require 'cl)
(load (concat user-emacs-directory "core/base"))
(load (concat user-emacs-directory "core/core"))
(org-babel-load-file (concat user-emacs-directory "core/emacs.org"))

(let ((private-file (concat user-emacs-directory "private/init.el")))
  (when (file-exists-p private-file)
    (message "Load private files...")
    (load private-file)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-highlighting-mode 'symbols)
 '(flycheck-indication-mode nil)
 '(git-link-open-in-browser t)
 '(git-messenger:use-magit-popup t)
 '(org-agenda-files
   '("~/org-modes/personal.org" "~/org-modes/employmenthero.org"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-protocol org-rmail org-w3m))
 '(plantuml-default-exec-mode 'jar)
 '(projectile-require-project-root nil)
 '(ruby-end-insert-newline nil)
 '(safe-local-variable-values
   '((org-drill-learn-fraction . 0.2)
     (org-drill-spaced-repetition-algorithm . simple8)
     (org-drill-maximum-items-per-session . 50)))
 '(which-key-prefix-prefix "+")
 '(which-key-separator " "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
