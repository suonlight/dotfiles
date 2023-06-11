(use-package! lsp-bridge
  :config
  (setq acm-enable-quick-access t)
  (setq acm-quick-access-modifier 'meta)

  (map! :map acm-mode-map
    "C-n"           #'acm-select-next
    "C-p"           #'acm-select-prev
    [tab]           #'acm-select-next
    [backtab]       #'acm-select-prev)

  (map!
    (:map org-mode-map
    :i "C-n"           #'acm-select-next
    :i "C-p"           #'acm-select-prev
    :i "C-e"           #'acm-complete))

  (map! :map typescript-tsx-mode-map
    :i "C-n"           #'acm-select-next
    :i "C-p"           #'acm-select-prev
    :i [tab]           #'acm-select-next
    :i [backtab]       #'acm-select-prev)

  (map! :map doom-leader-code-map
    "r"             #'lsp-bridge-rename
    "t"             #'lsp-bridge-find-type-def
    "x"             #'lsp-bridge-diagnostic-list)

  (set-lookup-handlers! '(ruby-mode ruby-ts-mode typescript-mode typescript-tsx-mode tsx-ts-mode typescript-ts-mode)
    :definition #'lsp-bridge-find-def
    :references #'lsp-bridge-find-references
    :documentation #'lsp-bridge-popup-documentation)

  (map! :map lsp-bridge-ref-mode-map
    :nv "q" #'lsp-bridge-ref-quit
    :nv "j" #'lsp-bridge-ref-jump-next-keyword
    :nv "k" #'lsp-bridge-ref-jump-prev-keyword
    :nv "RET" #'lsp-bridge-ref-open-file-and-stay)

  (map! :localleader
    (:map lsp-bridge-ref-mode-map
      "e" #'lsp-bridge-ref-switch-to-edit-mode))

  (push "ruby" lsp-bridge-org-babel-lang-list)
  (push "javascript" lsp-bridge-org-babel-lang-list)

  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))
