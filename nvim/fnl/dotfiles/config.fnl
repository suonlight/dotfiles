(module dotfiles.config
  {autoload {nvim aniseed.nvim
             nvim-util aniseed.nvim.util
             util dotfiles.util
             telescope telescope
             actions telescope.actions
             orgmode orgmode
             lsp lspconfig}
   require-macros [dotfiles.macros]})

;; default
(_: colorscheme :doom-one)
(set nvim.o.termguicolors true)
(set nvim.o.clipboard :unnamed)
(set nvim.o.autoindent true)
(set nvim.o.smartindent true)
(set nvim.o.expandtab true)
(set nvim.o.softtabstop 2)
(set nvim.o.shiftwidth 2)
(set nvim.o.number false)
(set nvim.o.relativenumber false)
(set nvim.o.smartcase true)
(set nvim.o.encoding :UTF-8)

;; ale
(set nvim.g.ale_linters
  {:javascript [:eslint]
   :ruby [:rubocop]
   :clojure [:clj-kondo :joker]})

(set nvim.g.ale_fixers
  {:javascript ["prettier" "eslint"]
   :ruby [:rubocop]})

(set nvim.g.ale_linters_explicit 1)
(set nvim.g.ale_completion_enabled 1)
(set nvim.g.ale_lint_on_save 1)
(set nvim.g.ale_lint_on_text_changed "never")
(set nvim.g.ale_echo_cursor 1)
(set nvim.g.ale_echo_msg_error_str "E")
(set nvim.g.ale_echo_msg_warning_str "W")
(set nvim.g.ale_echo_msg_format "[%linter%] %s [%severity%]")
(set nvim.g.ale_set_highlights 0)
(set nvim.g.ale_set_loclist 0)
(set nvim.g.ale_set_quickfix 1)
(set nvim.g.ale_fix_on_save 1)

;; telescope
(telescope.setup {:defaults {:mappings {:i {"<esc>" actions.close}}}})

;; sexp
(set nvim.g.sexp_filetypes "clojure,scheme,lisp,fennel")

;; orgmode
(orgmode.setup {})

;; textobj-entire
; (nvim.command "call textobj#user#plugin('entire', {
; \      '-': {
; \        'select-a': 'ag',  'select-a-function': 'textobj#entire#select_a',
; \        'select-i': 'ig',  'select-i-function': 'textobj#entire#select_i'
; \      }
; \    })
; ")

;; prettier
(vim.schedule
  (fn []
    (autocmd
      :BufWritePre
      "*.js,*.jsx,*.mjs,*.ts,*.tsx,*.less,*.json,*.graphql,*.md,*.vue"
      "Prettier")))

;; lightline
(fn->viml :dotfiles.util :filename :LightlineFilename)
(fn->viml :dotfiles.util :readonly :LightlineReadonly)

(set nvim.g.lightline
     {:colorscheme :default
      :component_function {:filename :LightlineFilename
                           :readonly :LightlineReadonly}
      :active {:left [[:mode :paste]
                      [:readonly :filename :modified]]
               :right [[:lineinfo]
                       [:percent]]}
      :inactive {:left [[:filename]]
                 :right []}})

;; lsp javascript
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
                   (_: "command! LspDef lua vim.lsp.buf.definition()")
                   (_: "command! LspHover lua vim.lsp.buf.hover()")
                   (noremap-buffer bufnr :n :gd "<cmd>LspDef<CR>")
                   (noremap-buffer bufnr :n :K "<cmd>LspHover<CR>")))

; (nvim.command "lua require('lspconfig').tsserver.setup{}")

; (lsp.diagnosticls.setup {:on_attach (fn [client]
;                                       ; (local client.resolved_capabilities.document_formatting false)
;                                       (on-attach client))
;                          :filetypes [:typescript :javascript]
;                          :init_options {:filetypes file-types
;                                         :linters linters
;                                         :formatters formatters
;                                         :formatFiletypes format-file-types}})

(lsp.tsserver.setup {:on_attach (fn [client bufnr]
                                  (set client.resolved_capabilities.document_formatting false)
                                  (on-attach client bufnr))})

