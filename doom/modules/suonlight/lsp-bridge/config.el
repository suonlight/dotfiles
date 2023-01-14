(use-package! lsp-bridge
  :config
  (map! :map acm-mode-map
    [tab]           #'acm-select-next
    [backtab]       #'acm-select-prev)
  (map! :map doom-leader-code-map
    :desc "LSP Rename"
    "r"             #'lsp-bridge-rename
    :desc "LSP Find declaration"
    "j"             #'lsp-bridge-find-def)
  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))
