(module dotfiles.modules.javascript
 {autoload {nvim aniseed.nvim
             a aniseed.core
             lsp lspconfig}
   require-macros [dotfiles.macros]})

(local file-types {:typescript "eslint" :javascript "eslint"})

(local linters {:eslint {:sourceName :eslint
                         :command :eslint_d
                         :rootPatterns [".eslintrc.js" "package.json"]
                         :debounce 100
                         :args ["--stdin" "--stdin-filename" "%filepath" "--format" "json"]
                         :parseJson {:errorsRoot "[0].messages"
                                     :line :line
                                     :column :column
                                     :endLine :endLine
                                     :endColumn :endColumn
                                     :message "${message} [${ruleId}]"
                                     :security "severity"}
                         :securities {[2] "error"
                                      [1] "warning"}}})

(local formatters {:prettier {:command :prettier :args ["--stdin-filepath" "%filepath"]}})

(local format-file-types {:typescript "prettier" :javascript "prettier"})

(local on-attach (fn [client bufnr]
                   (nvim.command "command! LspDef lua vim.lsp.buf.definition()")
                   (nvim.command "command! LspHover lua vim.lsp.buf.hover()")
                   (nvim.buf_set_keymap bufnr "n" "gd" "<cmd>LspDef<CR>" {:silent true})
                   (nvim.buf_set_keymap bufnr "n" "K" "<cmd>LspHover<CR>" {:silent true})))

(defn setup []
  ; (nvim.command "lua require('lspconfig').tsserver.setup{}")
  (lsp.tsserver.setup {:on_attach (fn [client bufnr]
                                    (set client.resolved_capabilities.document_formatting false)
                                    (on-attach client bufnr))})

  ; (lsp.diagnosticls.setup {:on_attach (fn [client]
  ;                                       ; (local client.resolved_capabilities.document_formatting false)
  ;                                       (on-attach client))
  ;                          :filetypes [:typescript :javascript]
  ;                          :init_options {:filetypes file-types
  ;                                         :linters linters
  ;                                         :formatters formatters
  ;                                         :formatFiletypes format-file-types}})

  (print "Setup tsserver!"))

(setup)
