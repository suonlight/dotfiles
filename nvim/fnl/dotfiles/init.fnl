(module dotfiles.init
  {autoload {nvim aniseed.nvim
             a aniseed.core
             nvim-util aniseed.nvim.util
             util dotfiles.util
             which-key which-key
             lsp lspconfig}
   require-macros [dotfiles.macros]})

;; Plugins to be managed by packer.
(util.use
  :Olical/aniseed {}
  :Olical/nvim-local-fennel {}
  :Olical/conjure {}

  ; defaults
  :editorconfig/editorconfig-vim {}
  :folke/which-key.nvim {}
  :mhinz/vim-startify {}
  :windwp/nvim-autopairs {}
  :yggdroot/indentLine {}
  :danro/rename.vim {}
  :phaazon/hop.nvim {} ; easy motion
  :tpope/vim-commentary {}
  :tpope/vim-endwise {}
  :pechorin/any-jump.vim {}
  :nvim-treesitter/nvim-treesitter {:run ":TSUpdate"}  ; We recommend updating the parsers on update

  ; startup time
  :lewis6991/impatient.nvim {}

  ; search files/keyword
  ; :nvim-lua/popup.nvim {}
  :nvim-lua/plenary.nvim {}
  :vijaymarupudi/nvim-fzf {}
  :ibhagwan/fzf-lua {}

  ; tmux
  :christoomey/vim-tmux-navigator {}
  :benmills/vimux {}

  ; text objects
  :kana/vim-textobj-user {}
  :michaeljsmith/vim-indent-object {}
  :kana/vim-textobj-line  {}
  :kana/vim-textobj-entire  {}
  :tpope/vim-surround {}

  ; git
  :tpope/vim-fugitive {}
  :tpope/vim-rhubarb {}
  :sindrets/diffview.nvim {}
  :TimUntersberger/neogit {}

  ; ui
  :joshdick/onedark.vim {}
  :folke/tokyonight.nvim {:opt true}
  :kyazdani42/nvim-web-devicons {}
  :kyazdani42/nvim-tree.lua {}
  ; :yamatsum/nvim-nonicons {}
  ; :itchyny/lightline.vim {}
  :glepnir/galaxyline.nvim {:branch :main}
  :andymass/vim-matchup {}
  :rcarriga/nvim-notify {}

  ; lisp
  :guns/vim-sexp {:opt true}

  ; linter
  :dense-analysis/ale {}

  ; javascript
  :prettier/vim-prettier {}
  :pangloss/vim-javascript {}
  :maxmellon/vim-jsx-pretty {}
  :alvan/vim-closetag {}

  ; ruby
  :tpope/vim-projectionist {} ; take ~ 200ms to startup
  :janko-m/vim-test {}

  ; lsp
  :williamboman/mason.nvim {}
  :williamboman/mason-lspconfig.nvim {}
  :neovim/nvim-lspconfig {}
  ;; :github/copilot.vim {}

  ; notes
  :kristijanhusak/orgmode.nvim {}
  :akinsho/org-bullets.nvim {}
  :michaelb/sniprun {:run "bash install.sh"}

  ; completion
  :hrsh7th/nvim-compe {}

  ; config
  :dstein64/vim-startuptime {}

  ;; TODO research each plugin
  ; :tpope/vim-abolish {}
  ; :tpope/vim-dadbod {}
  ; :tpope/vim-dispatch {}
  ; :tpope/vim-eunuch {}
  ; :tpope/vim-repeat {}
  ; :tpope/vim-sexp-mappings-for-regular-people {}
  ; :tpope/vim-sleuth {}
  ; :tpope/vim-unimpaired {}
  ; :tpope/vim-vinegar {}
  ; :Olical/vim-enmasse {}
  ; :PeterRincker/vim-argumentative {}
  ; :airblade/vim-gitgutter {}
  ; :clojure-vim/vim-jack-in {}
  ; :dag/vim-fish {}
  ; :hylang/vim-hy {}
  ; :lambdalisue/suda.vim {}
  ; :liuchengxu/vim-better-default {:mod :better-default}
  ; :mbbill/undotree {:mod :undotree}
  ; :norcalli/nvim-colorizer.lua {:mod :colorizer}
  ; :radenling/vim-dispatch-neovim {}
  ; :wlangstroth/vim-racket {}
  )

(require :impatient)

;; default
; (ex colorscheme :onedark)
(vim.defer_fn (fn [] (ex colorscheme :onedark)) 1)
(set nvim.o.termguicolors true)
(set nvim.o.clipboard :unnamed)
(set nvim.o.autoindent true)
(set nvim.o.smartindent true)
(set nvim.o.expandtab true)
(set nvim.o.softtabstop 2)
(set nvim.o.shiftwidth 2)
(set nvim.o.number false)
(set nvim.o.relativenumber false)
(set nvim.o.encoding :UTF-8)
(set nvim.o.hlsearch true) ; enable search result highlighting
(set nvim.o.ignorecase true)
(set nvim.o.smartcase true)
(set nvim.o.wrap false) ; nowrap
(set nvim.o.ttyfast true)
(set nvim.o.lazyredraw true)

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
;; (set nvim.g.ale_set_quickfix 1)
(set nvim.g.ale_fix_on_save 1)

(set nvim.g.nvim_tree_icons
     {:default ""
     :symlink ""
     :git {:unstaged "✗"
           :staged "✓"
           :unmerged ""
           :renamed "➜"
           :untracked "★"
           :deleted ""
           :ignored "◌" }
     :folder {:arrow_open ""
              :arrow_closed ""
              :default ""
              :open ""
              :empty ""
              :empty_open ""
              :symlink ""
              :symlink_open ""}
     })

;; indentLine
(set nvim.g.indentLine_enabled 0)
(set nvim.g.indentLine_concealcursor "inc")
(set nvim.g.indentLine_conceallevel 2)

;; sexp
(set nvim.g.sexp_filetypes "clojure,scheme,lisp,fennel")

;; async setup
(vim.schedule
  (fn []
    ;; hop
    (let [hop (require :hop)]
      (hop.setup {:keys "etovxqpdygfblzhckisuran"}))

    ;; nvim-web-devicons
    (let [devicons (require :nvim-web-devicons)]
      (devicons.setup {:default true}))

    (let [ntree (require :nvim-tree)]
      (ntree.setup {}))

    ;; fzf-lua
    (let [fzf-lua (require :fzf-lua)]
      (fzf-lua.setup {:winopts {:split "belowright new"}}))

    ;; modeline
    (let [modeline (require :dotfiles.modeline)]
      (modeline.setup))

    ;; autopairs
    (let [autopairs (require :nvim-autopairs)]
      (autopairs.setup {})
      (let [autopairs-compe (require :nvim-autopairs.completion.compe)]
        (autopairs-compe.setup {:map_cr true :map_complete true })))

    ;; compe
    (let [compe (require :compe)]
      (compe.setup
        {:enabled true
         :autocomplete true
         :debug false
         :min_length 1
         :preselect "enable"
         :throttle_time 80
         :source_timeout 200
         :incomplete_delay 400
         :max_abbr_width 100
         :max_kind_width 100
         :max_menu_width 100
         :documentation true
         :source {:path true
                  :buffer true
                  :calc true
                  :nvim_lsp true
                  :nvim_lua true
                  :conjure true
                  :vsnip false}})) ; TODO handle vsnip with TAB

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

    (defn on-attach [client bufnr]
      ; (_: "command! LspDef lua vim.lsp.buf.definition()")
      ; (_: "command! LspHover lua vim.lsp.buf.hover()")
      (noremap-buffer bufnr :n :gD "<cmd>lua vim.lsp.buf.declaration()<CR>" {:noremap true :silent true})
      (noremap-buffer bufnr :n :gd "<cmd>lua vim.lsp.buf.definition()<CR>" {:noremap true :silent true})
      (noremap-buffer bufnr :n :K "<cmd>lua vim.lsp.buf.hover()<CR>" {:noremap true :silent true})
      ;; (noremap-buffer bufnr :n :gi "<cmd>lua vim.lsp.buf.implementation()<cR>" {:noremap true :silent true})
      (noremap-buffer bufnr :n :<C-k> "<cmd>lua vim.lsp.buf.signature_help()<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>wa "<cmd>lua vim.lsp.buf.add_workspace_folder()<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>wr "<cmd>lua vim.lsp.buf.remove_workspace_folder()<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>wl "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>D "<cmd>lua vim.lsp.buf.type_definition()<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>rn "<cmd>lua vim.lsp.buf.rename()<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>e "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cR>" {:noremap true :silent true})
      ; (noremap-buffer bufnr :n :<space>q "<cmd>lua vim.lsp.diagnostic.set_loclist()<cR>" {:noremap true :silent true})
      (noremap-buffer bufnr :n :gr "<cmd>lua vim.lsp.buf.references()<cR>" {:noremap true :silent true})
      (noremap-buffer bufnr :n "[d" "<cmd>lua vim.lsp.diagnostic.goto_prev()<cR>" {:noremap true :silent true})
      (noremap-buffer bufnr :n "]d" "<cmd>lua vim.lsp.diagnostic.goto_next()<cR>" {:noremap true :silent true}))

    ; (lsp.diagnosticls.setup {:on_attach (fn [client bufnr]
    ;                                       ; (local client.resolved_capabilities.document_formatting false)
    ;                                       (on-attach client bufnr))
    ;                          :filetypes [:typescript :javascript]
    ;                          :init_options {:filetypes file-types
    ;                                         :linters linters
    ;                                         :formatters formatters
    ;                                         :formatFiletypes format-file-types}})


    ;; mason
    (let [mason (require :mason)
          mason-lspconfig (require :mason-lspconfig)]
      (mason.setup {})
      (mason-lspconfig.setup {:ensure_installed ["solargraph" "tsserver"]})
      (lsp.solargraph.setup {:on_attach on-attach})
      (lsp.tsserver.setup {:on_attach on-attach}))

    ;; neogit
    (let [neogit (require :neogit)]
      ;; (neogit.config.use_magit_keybindings)
      (neogit.setup {:integrations {:diffview true}
                     :disable_hint true
                     :disable_commit_confirmation true}))

    (let [diffview (require :diffview)]
      (diffview.setup {}))

    ;; org mode
    (let [parser (require :nvim-treesitter.parsers)
          configs (require :nvim-treesitter.configs)
          sniprun (require :sniprun)
          orgmode (require :orgmode)]

      (configs.setup {:highlight {:enable true
                                  :disable ["org"]
                                  :additional_vim_regex_highlighting ["org"]}
                      :matchup {:enable true
                                :include_match_words true}
                      :ensure_installed ["org"]})
      (sniprun.setup {:display ["Classic" "NvimNotify"]
                      :display_options {:notification_timeout 10}})
      (orgmode.setup_ts_grammar)
      (orgmode.setup {})))

    (let [orgbullets (require :org-bullets)]
      (orgbullets.setup {:concealcursor true
                         :symbols {:headlines ["◉" "○" "✸" "✿"]}})))

;; textobj-entire
; (nvim.command "call textobj#user#plugin('entire', {
; \      '-': {
; \        'select-a': 'ag',  'select-a-function': 'textobj#entire#select_a',
; \        'select-i': 'ig',  'select-i-function': 'textobj#entire#select_i'
; \      }
; \    })
; ")

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

;; closetag
(set nvim.g.closetag_filenames "*.html,*.xhtml,*.phtml,*.erb,*.jsx,*.js")
(set nvim.g.closetag_xhtml_filenames "*.xhtml,*.jsx,*.erb,*.js")
(set nvim.g.closetag_emptyTags_caseSensitive 1)
(set nvim.g.closetag_close_shortcut "<leader>>") ; Add > at current position without closing the current tag, default is ''

;; Startify
(set nvim.g.startify_change_to_vcs_root 1)

;; tmux
(set nvim.g.tmux_navigator_no_mappings 1)
(set nvim.g.tmux_navigator_save_on_switch 1)

;; vim-test
(set nvim.g.test#strategy "vimux")
(set nvim.g.test#preserve_screen 1)

(set nvim.test#enabled_runners ["ruby#rspec"])
(set nvim.test#ruby#minitest#file_pattern "_spec.rb")

;; nvim-compe
(set nvim.o.completeopt "menuone,noselect")

(defn- replace-termcodes [str]
  (nvim.replace_termcodes str true true true))

(defn- check-backspace []
  (let [col (- (nvim.fn.col ".") 1)
        space-under-cursor? (-> (nvim.fn.getline ".")
                                  (string.sub col col)
                                  (string.match "%s")
                                  (not= nil))]
    (or (= col 0) space-under-cursor?)))

(global tab_complete (fn []
                    (if (= (nvim.fn.pumvisible) 1)
                      (replace-termcodes "<C-n>")
                      (if (check-backspace)
                        (replace-termcodes "<Tab>")
                        ((. nvim.fn "compe#complete"))))))

(global s_tab_complete (fn []
                    (if (= (nvim.fn.pumvisible) 1)
                      (replace-termcodes "<C-p>")
                      (replace-termcodes "<S-Tab>"))))

(imap :<Tab> "v:lua.tab_complete()" {:expr true})
(imap :<S-Tab> "v:lua.s_tab_complete()" {:expr true})

;; custom commands
(fn->viml :dotfiles.util :gh-open-pull-request :GhOpenPullRequest)
(fn->viml :dotfiles.util :gh-list-pull-requests :GhListPullRequests)
(fn->viml :dotfiles.util :ci-open :CiOpen)
(fn->viml :dotfiles.util :js-insert-i18n :JsInsertI18n)
(fn->viml :dotfiles.util :org-roam-dailies-find-today :OrgRoamDailiesFindToday)
(noremap :n :gog "<cmd>call GhOpenPullRequest()<CR>" {:silent true})
(noremap :n :goc "<cmd>call CiOpen()<CR>" {:silent true})

;; bindings
(which-key.register
  {:/ ["<cmd>FzfLua live_grep<CR>" "Search project"]
   :* ["<cmd>FzfLua grep_cword<CR>" "Search at point"]
   :<tab> ["<C-^>" "Switch to last buffer"]
   :q {:name "+quit/session"
       :q ["<cmd>q<CR>" "Quit vim"]}
   :p {:name "+projects"
       :f ["<cmd>FzfLua files<CR>" "Find file"]
       :g ["<cmd>FzfLua tags<CR>" "Find Tags"]
       :a ["<cmd>A<CR>" "Toggle implementation and test"]}
   :f {:name "+files"
       :s ["<cmd>update<CR>" "File save"]
       :f ["<cmd>NvimTreeFindFile<CR>" "Find file in directory"]
       :t ["<cmd>NvimTreeToggle<CR>" "Toggle Tree"]
       :R [":Rename " "Rename file"]
       :c [":saveas <C-R>=expand(\"%:p:h\")<CR>/" "Copy file"]
       :r ["<cmd>FzfLua oldfiles<CR>" "Recent files"]
       :y [":let @*=expand('%:p') | echo @*<CR>" "Copy Full File Path"]}
   :g {:name "+git"
       :p ["<cmd>call GhListPullRequests()<CR>" "Github List PRs"]
       :oo ["<cmd>Gbrowse<CR>" "Git browse"]
       :s ["<cmd>Neogit<CR>" "Git status"]
       :b ["<cmd>Git blame<CR>" "Git blame"]}
   :b {:name "+buffers"
       :b ["<cmd>FzfLua buffers<CR>" "Find buffer"]
       :d ["<cmd>bdelete<CR>" "Delete buffer"]
       :n ["<cmd>bdelete<CR>" "Next buffer"]
       :p ["<cmd>bdelete<CR>" "Previous buffer"]
       :h ["<cmd>Startify<CR>" "Home buffer"]}
   :w {:name "+windows"
       :h ["<cmd>wincmd h<CR>" "Window left"]
       :l ["<cmd>wincmd l<CR>" "Window right"]
       :j ["<cmd>wincmd j<CR>" "Window down"]
       :k ["<cmd>wincmd k<CR>" "Window up"]
       :<S-h> ["<cmd>wincmd <S-h><CR>" "Move window far left"]
       :<S-l> ["<cmd>wincmd <S-l><CR>" "Move window far right"]
       :<S-j> ["<cmd>wincmd <S-j><CR>" "Move window very down"]
       :<S-k> ["<cmd>wincmd <S-k><CR>" "Move window very top"]
       :w ["<cmd>wincmd w<CR>" "Other window"]
       := ["<cmd>wincmd =<CR>" "Window balance area"]
       :r ["<cmd>wincmd r<CR>" "Rotate window"]
       :s ["<cmd>wincmd s<CR>" "Window split"]
       :v ["<cmd>wincmd v<CR>" "Window vsplit"]
       :c ["<cmd>close<CR>" "Window close"]}
   :s {:name "+search"
       :p ["<cmd>FzfLua live_grep<CR>" "Search in project"]
       :s ["<cmd>FzfLua grep_curbuf<CR>" "Search in buffer"]}
   :j {:name "+jump"
       :j ["<cmd>HopChar1<CR>" "Jump to char"]
       :w ["<cmd>HopWord<CR>" "Jump to word"]
       :l ["<cmd>HopLine<CR>" "Jump to line"]}
   :r {:name "+registers"
       :e ["<cmd>FzfLua registers<CR>" "Registers"]}
   :t {:name "+toggle"
       :l ["<cmd>set nu! rnu!<CR>" "Toggle Line Number"]
       :i ["<cmd>IndentLinesToggle<CR>" "Toggle indent line"]}
   :n {:name "+notes"
       :r {:name "+roam"
           :d {:name "+date"
               :t ["<cmd>call OrgRoamDailiesFindToday()<CR>" "org-roam-dailies-find-today"]}}}
   :h {:name "+help"
       :? ["<cmd>FzfLua help_tags<CR>" "Help tags"]
       :e ["<cmd>messages<CR>" "View messages"]
       :df ["<cmd>FzfLua commands<CR>" "Help Commands"]
       :t ["<cmd>FzfLua colorschemes<CR>" "Load theme"]}}
  {:prefix "<leader>"})

;; Generic mapping configuration.
(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

(noremap :n :<space> :<nop>)
(noremap :n :<M-s> "<cmd>update<CR>")
(noremap :n :<M-b> "<cmd>FzfLua buffers<CR>")
(noremap :n :<M-w> "<cmd>close<CR>")
(noremap :n :<C-p> "<cmd>FzfLua files<CR>")

;; windows
(noremap :n :<C-h> "<cmd>TmuxNavigateLeft<CR>")
(noremap :n :<C-j> "<cmd>TmuxNavigateDown<CR>")
(noremap :n :<C-k> "<cmd>TmuxNavigateUp<CR>")
(noremap :n :<C-l> "<cmd>TmuxNavigateRight<CR>")

(noremap :n :<Leader>! "yy:let cliptext = getreg('*') | :VimuxRunCommand(cliptext)<CR><CR>")
(noremap :v :<Leader>! "y:let cliptext = getreg('*') | :VimuxRunCommand(cliptext)<CR><CR>")

(noremap :n :<Leader>0 "<cmd>NvimTreeFindFile<CR>")
(noremap :n :<f5> ":TestNearest<CR>:TmuxNavigateDown<CR>")

(nmap :s "<cmd>HopChar1<CR>")
(nmap :gy "yygccp")

;; ALE
(noremap :n "]e" "<cmd>ALENext<CR>")
(noremap :n "[e" "<cmd>ALEPrevious<CR>")
(noremap :n :<Leader>ef "<cmd>ALEFix<CR>")

(augroup
  :FileFugitive
  (autocmd :FileType :fugitive "nmap <buffer> q gq")
  (autocmd :FileType :fugitive "nmap <buffer> pp :Git push<CR>")
  (autocmd :FileType :fugitiveblame "nmap <buffer> q gq")
  (autocmd :FileType :gitcommit "imap <buffer> <C-c><C-c> :wq<CR>")
  (autocmd :FileType :gitcommit "nmap <buffer> <C-c><C-c> :wq<CR>")
  (autocmd :FileType :gitcommit "imap <buffer> <C-c><C-k> :q!<CR>")
  (autocmd :FileType :gitcommit "nmap <buffer> <C-c><C-k> :q!<CR>"))

(augroup
  :FileRuby
  (autocmd :FileType :ruby "noremap <f5> :TestNearest<CR>:TmuxNavigateDown<CR>")
  (autocmd :FileType :ruby "nnoremap <LocalLeader>tt :TestNearest<CR>:TmuxNavigateDown<CR>")
  (autocmd :FileType :ruby "nnoremap <LocalLeader>tb :TestFile<CR>:TmuxNavigateDown<CR>"))

(augroup
  :FileJavascript
  (autocmd :FileType :javascript "nnoremap <LocalLeader>il :call JsInsertI18n()<CR>"))

(augroup
  :FileOrgMode
  (autocmd :FileType :org "nmap <buffer> <C-c><C-c> :SnipRun<CR>")
  (autocmd :FileType :org "nmap <buffer> <LocalLeader>, :SnipRun<CR>"))

(augroup
  :Prettier
  (autocmd
    :BufWritePre
    "*.js,*.jsx,*.mjs,*.ts,*.tsx,*.less,*.json,*.graphql,*.md,*.vue"
    "Prettier"))

;; not work
(inoremap :<C-Space> "compe#complete()" {:silent true :expr true})
(inoremap :<C-e> "compe#close('<C-e>')" {:silent true :expr true})
(inoremap :<C-f> "compe#scroll({ 'delta': +4 })" {:silent true :expr true})
(inoremap :<C-d> "compe#scroll({ 'delta': -4 })" {:silent true :expr true})
