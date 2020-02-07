(use-package yaml-mode
  :mode "\\.ya?ml$"
  :config
  (yaml-imenu-enable))

(use-package yaml-imenu)

(setq plantuml-jar-path "~/org-modes/plantuml.jar")
(use-package plantuml-mode
  :commands plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar))

(use-package apib-mode
  :mode ("\\.apib\\'" . apib-mode))

(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode))

(use-package terraform-mode)

(use-package protobuf-mode)

(use-package company-terraform
  :after company
  :straight (emacs-company-terraform :type git :host github :repo "rafalcieslak/emacs-company-terraform")
  :config (company-terraform-init))

(use-package lua-mode)
