(use-package rust-mode
  :hook (rust-mode . lsp)
  :defer t
  :hook (rust-mode . yas-minor-mode)
  :config (setq rust-format-on-save t))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :after rust-mode
  :hook ((rust-mode . racer-mode)
	 (racer-mode . eldoc-mode)
	 (racer-mode . company-mode))
  :config (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))
