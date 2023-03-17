(use-package! treesit-auto
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt)

  (push '(ruby-mode . ruby-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
  (push '(typescript-tsx-mode . tsx-ts-mode) major-mode-remap-alist)
  (push '(rjsx-mode . js-ts-mode) major-mode-remap-alist))

(after! lsp-bridge
  (push '(ruby-ts-mode . "solargraph") lsp-bridge-single-lang-server-mode-list)

  (add-hook! ruby-ts-mode #'lsp-bridge-mode)
  (add-hook! tsx-ts-mode #'lsp-bridge-mode))
