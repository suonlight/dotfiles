;;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :defer 1
  :config (load-theme 'doom-one t))

(use-package kaolin-themes
  :defer 1)

(use-package doom-modeline
  :after doom-themes
  :init
  (setq doom-modeline-height 20
    doom-modeline-bar-width 3
    doom-modeline-buffer-file-name-style 'relative-to-project
    doom-modeline-minor-modes nil
    doom-modeline-major-mode-color-icon t
    doom-modeline-icon t)
  :config
  (doom-modeline-mode))

(use-package all-the-icons
  :defer 2
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
	       '(inf-ruby-mode all-the-icons-octicon "ruby" :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
                 '(ruby-mode all-the-icons-octicon "ruby" :face all-the-icons-lred)))

(use-package font-lock+
  :straight
  (font-lock+ :repo "emacsmirror/font-lock-plus" :host github :type git))

(use-package all-the-icons-dired
  :defer 2
  :hook (dired-mode . all-the-icons-dired-mode)
  :after all-the-icons
  :config
  (advice-add #'all-the-icons-dired--display :before #'all-the-icons-dired--reset))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :config
  (all-the-icons-ivy-setup))

(defconst cycle-themes '(doom-solarized-light doom-one wombat))
(defvar current-theme (car cycle-themes))

(defun next-cycle-theme ()
  (interactive)
  (let ((next-theme (cdr (memq current-theme cycle-themes))))
    (if next-theme
	(setq current-theme (car next-theme))
      (setq current-theme (car cycle-themes)))
    (message "Loading theme: %s" current-theme)
    (counsel-load-theme-action (symbol-name current-theme))))
