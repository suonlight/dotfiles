(use-package! copilot
  :hook ((prog-mode . copilot-mode)
          (git-commit-mode . copilot-mode)
          (org-mode . copilot-mode))
  :bind (:map copilot-completion-map
          ("C-TAB" . 'copilot-accept-completion-by-word)
          ("C-<tab>" . 'copilot-accept-completion-by-word)
          ("<tab>" . 'copilot-accept-completion)
          ("TAB" . 'copilot-accept-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))
