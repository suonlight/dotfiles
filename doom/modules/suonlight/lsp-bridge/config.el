(use-package! lsp-bridge
  :config
  (map! :map acm-mode-map
    "C-n"           #'acm-select-next
    "C-p"           #'acm-select-prev
    [tab]           #'acm-select-next
    [backtab]       #'acm-select-prev)
  (map! :map typescript-tsx-mode-map
    :i "C-n"           #'acm-select-next
    :i "C-p"           #'acm-select-prev
    :i [tab]           #'acm-select-next
    :i [backtab]       #'acm-select-prev)
  (map! :map doom-leader-code-map
    :desc "LSP Rename"
    "r"             #'lsp-bridge-rename
    :desc "LSP Find declaration"
    "j"             #'lsp-bridge-find-def)

  (push "ruby" lsp-bridge-org-babel-lang-list)
  (push "javascript" lsp-bridge-org-babel-lang-list)

  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))
