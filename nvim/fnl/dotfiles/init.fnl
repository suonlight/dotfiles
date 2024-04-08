(module dotfiles.init
  {autoload {nvim aniseed.nvim
             a aniseed.core
             nvim-util aniseed.nvim.util
             util dotfiles.util
             which-key which-key
             lsp lspconfig}
   require-macros [dotfiles.macros]})

(use-package! :Olical/aniseed)
(use-package! :Olical/nvim-local-fennel :ft "fennel")
(use-package! :Olical/conjure :ft "fennel")

; defaults
(use-package! :editorconfig/editorconfig-vim :event "VeryLazy")
(use-package! :folke/which-key.nvim :lazy true)
(use-package! :mhinz/vim-startify
              :cmd "Startify"
              :config
              (fn [] (set nvim.g.startify_change_to_vcs_root 1)))
(use-package! :windwp/nvim-autopairs
              :lazy true
              :config
              (fn []
                (let [autopairs (require :nvim-autopairs)]
                  (autopairs.setup {})
                  (let [autopairs-compe (require :nvim-autopairs.completion.compe)]
                    (autopairs-compe.setup {:map_cr true :map_complete true })))))

(use-package! :yggdroot/indentLine :cmd "IndentLinesToggle"
              :config
              (fn []
                (set nvim.g.indentLine_enabled 0)
                (set nvim.g.indentLine_concealcursor "inc")
                (set nvim.g.indentLine_conceallevel 2)))

(use-package! :danro/rename.vim :cmd "Rename")
(use-package! :phaazon/hop.nvim ; easy motion
              :cmd ["HopChar1MW" "HopWordMW" "HopLine"]
              :config
              (fn []
                (let [hop (require :hop)]
                  (hop.setup {:keys "etovxqpdygfblzhckisuran"}))))
(use-package! :tpope/vim-commentary)
(use-package! :tpope/vim-endwise)
(use-package! :pechorin/any-jump.vim :lazy true
              :cmd ["AnyJump"]
              :config
              (fn []
                (set nvim.g.any_jump_references_enabled 0)))
(use-package! :nvim-treesitter/nvim-treesitter :build ":TSUpdate")  ; We recommend updating the parsers on update

; search files/keyword
; :nvim-lua/popup.nvim {}
(use-package! :nvim-lua/plenary.nvim)
(use-package! :vijaymarupudi/nvim-fzf :lazy true)

(use-package! :ibhagwan/fzf-lua
              :cmd "FzfLua"
              :dependencies ["nvim-tree/nvim-web-devicons" "vijaymarupudi/nvim-fzf"]
              :config
              (fn []
                (let [fzf-lua (require :fzf-lua)]
                  (fzf-lua.setup {:winopts {:split "belowright new"}}))))

(use-package! :christoomey/vim-tmux-navigator
              :cmd ["TmuxNavigateLeft" "TmuxNavigateDown" "TmuxNavigateUp" "TmuxNavigateRight"]
              :config
              (fn []
                (set nvim.g.tmux_navigator_no_mappings 1)
                (set nvim.g.tmux_navigator_save_on_switch 1)))

(use-package! :preservim/vimux)

; text objects
(use-package! :tpope/vim-surround)
(use-package! :chrisgrieser/nvim-various-textobjs
              :config
              (fn []
                (let [various-textobjs (require :various-textobjs)]
                  (various-textobjs.setup {:useDefaultKeymaps false})
                  (vim.keymap.set ["o" "x"] "ig" (fn [] (various-textobjs.entireBuffer)))
                  (vim.keymap.set ["o" "x"] "ag" (fn [] (various-textobjs.entireBuffer)))

                  (vim.keymap.set ["o" "x"] "ii" (fn [] (various-textobjs.indentation true true)))
                  (vim.keymap.set ["o" "x"] "ai" (fn [] (various-textobjs.indentation false true))))))


; git
(use-package! :tpope/vim-fugitive :cmd ["Git" "GBrowse"])
(use-package! :tpope/vim-rhubarb :cmd ["GBrowse"] :dependencies ["tpope/vim-fugitive"])
(use-package! :sindrets/diffview.nvim :lazy true)
(use-package! :TimUntersberger/neogit
              :cmd "Neogit"
              :dependencies ["sindrets/diffview.nvim"]
              :config
              (fn []
                (let [neogit (require :neogit)
                      diffview (require :diffview)]
                  (diffview.setup {})
                  (neogit.setup {:integrations {:diffview true}
                                 :use_magit_keybindings true
                                 :disable_hint true
                                 :disable_commit_confirmation true}))))

; ui
(use-package! :joshdick/onedark.vim :lazy true)
(use-package! :folke/tokyonight.nvim :lazy true)
(use-package! :nvim-tree/nvim-web-devicons )
(use-package! :nvim-tree/nvim-tree.lua
              :cmd ["NvimTreeFindFile" "NvimTreeToggle"]
              :dependencies ["nvim-tree/nvim-web-devicons"]
              :config
              (fn []
                (let [nvim-tree (require :nvim-tree)
                      glyphs {:default ""
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
                                       :symlink_open ""}}]
                  (nvim-tree.setup {:renderer {:icons {:glyphs glyphs}}}))))
; :yamatsum/nvim-nonicons {}
; :itchyny/lightline.vim {}
(use-package! :glepnir/galaxyline.nvim :branch :main)
(use-package! :andymass/vim-matchup)
(use-package! :rcarriga/nvim-notify :lazy true)

; lisp
(use-package! :guns/vim-sexp :lazy true
              :config (fn [] (set nvim.g.sexp_filetypes "clojure,scheme,lisp,fennel")))

; javascript
(use-package! :pangloss/vim-javascript :ft ["javascript"])
(use-package! :maxmellon/vim-jsx-pretty)
(use-package! :alvan/vim-closetag
              :config
              (fn []
                (set nvim.g.closetag_close_shortcut "<leader>>") ; Add > at current position without closing the current tag, default is ''
                (set nvim.g.closetag_filenames "*.html,*.xhtml,*.phtml,*.erb,*.jsx,*.js")
                (set nvim.g.closetag_xhtml_filenames "*.xhtml,*.jsx,*.erb,*.js")
                (set nvim.g.closetag_emptyTags_caseSensitive 1)))

(use-package! :rgroli/other.nvim
              :config
              (fn []
                (let [other-nvim (require :other-nvim)]
                  (other-nvim.setup
                    {:mappings ["rails" ;; example https://github.com/rgroli/other.nvim/blob/main/lua/other-nvim/builtin/mappings/rails.lua
                                {:pattern "spec/(.*)/(.*)_spec.rb$"
                                 :target "app/%1/%2.rb" :context "test"}]}))))

(use-package! :janko-m/vim-test
              :config
              (fn []
                (set nvim.g.test#strategy "vimux")
                (set nvim.g.test#preserve_screen 1)

                (set nvim.test#enabled_runners ["ruby#rspec"])
                (set nvim.test#ruby#minitest#file_pattern "_spec.rb")))

; lsp
(use-package! :williamboman/mason.nvim
              :config
              (fn []
                (defn on-attach [client bufnr]
                      ; (_: "command! LspDef lua vim.lsp.buf.definition()")
                      ; (_: "command! LspHover lua vim.lsp.buf.hover()")
                      (noremap-buffer bufnr :n :gD "<cmd>lua vim.lsp.buf.declaration()<CR>" {:noremap true :silent true})
                      (noremap-buffer bufnr :n :gd "<cmd>lua vim.lsp.buf.definition()<CR>" {:noremap true :silent true})
                      (noremap-buffer bufnr :n :K "<cmd>lua vim.lsp.buf.hover()<CR>" {:noremap true :silent true})
                      ;; (noremap-buffer bufnr :n :gi "<cmd>lua vim.lsp.buf.implementation()<cR>" {:noremap true :silent true})
                      (noremap-buffer bufnr :n :<C-k> "<cmd>lua vim.lsp.buf.signature_help()<CR>" {:noremap true :silent true})
                      (noremap-buffer bufnr :n :<space>ca "<cmd>lua vim.lsp.buf.code_action()<CR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>wa "<cmd>lua vim.lsp.buf.add_workspace_folder()<cR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>wr "<cmd>lua vim.lsp.buf.remove_workspace_folder()<cR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>wl "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>D "<cmd>lua vim.lsp.buf.type_definition()<cR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>rn "<cmd>lua vim.lsp.buf.rename()<cR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>e "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cR>" {:noremap true :silent true})
                      ; (noremap-buffer bufnr :n :<space>q "<cmd>lua vim.lsp.diagnostic.set_loclist()<cR>" {:noremap true :silent true})
                      (noremap-buffer bufnr :n :gr "<cmd>lua vim.lsp.buf.references()<cR>" {:noremap true :silent true}))

                (let [mason (require :mason)
                      mason-lspconfig (require :mason-lspconfig)]
                  (mason.setup {})
                  ; (mason-lspconfig.setup {:ensure_installed ["solargraph" "tsserver"]})
                  ; (lsp.grammarly.setup {:on_attach on-attach :filetypes ["org" "markdown"]})
                  ; (lsp.ltex.setup {:on_attach on-attach :filetypes ["org" "markdown"]})
                  (lsp.solargraph.setup {:on_attach on-attach
                                         :root_dir (lsp.util.root_pattern "Gemfile" ".git" ".")
                                         :cmd [(.. (os.getenv "HOME") "/.asdf/installs/ruby/2.7.8/bin/solargraph") "stdio"]})
                  (lsp.tsserver.setup {:on_attach on-attach}))))

(use-package! :williamboman/mason-lspconfig.nvim)
(use-package! :neovim/nvim-lspconfig)
(use-package! :mhartington/formatter.nvim
              :event "BufWritePost"
              :config
              (fn []
                (let [formatter (require :formatter)
                      filetypes (require :formatter.filetypes)
                      prettier  filetypes.typescript.prettier
                      rubocop   filetypes.ruby.rubocop]
                  (formatter.setup {:logging true
                                    :log_level vim.log.levels.WARN
                                    :filetype {:ruby             rubocop
                                               :typescriptreact  prettier
                                               :typescript       prettier
                                               :javascriptreact  prettier
                                               :javascript       prettier}}))))
(use-package! :mfussenegger/nvim-lint)
(use-package! :github/copilot.vim)
(use-package! :CopilotC-Nvim/CopilotChat.nvim
              ; :ft "canary"
              :dependencies ["github/copilot.vim" "nvim-lua/plenary.nvim"]
              :opts {:debug true}
              :config
              (fn []
                (let [copilot-chat (require :CopilotChat)]
                  (copilot-chat.setup {:debug true}))))

; notes
(use-package! :kristijanhusak/orgmode.nvim
              :ft "org"
              :dependencies ["nvim-treesitter/nvim-treesitter" "michaelb/sniprun"]
              :config
              (fn []
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
                  (orgmode.setup {:org_todo_keywords ["TODO" "DOING" "|" "DONE"]
                                  :mappings {:org {:org_todo "t"}}}))))

(use-package! :akinsho/org-bullets.nvim
              :dependencies ["kristijanhusak/orgmode.nvim"]
              :config
              (fn []
                (let [orgbullets (require :org-bullets)]
                  (orgbullets.setup
                   {:concealcursor true
                    :symbols {:headlines ["◉" "○" "✸" "✿"]}}))))

(use-package! :michaelb/sniprun :build "bash install.sh" :ft "org")
(use-package! :kkharji/sqlite.lua :ft "lua")

; completion
(use-package! :hrsh7th/nvim-compe
              :config
              (fn []
                (let [compe (require :compe)] ; TODO handle vsnip with TAB
                  (set nvim.o.completeopt "menuone,noselect")
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
                             :vsnip false}}))))

; config
(use-package! :dstein64/vim-startuptime :cmd "StartupTime")

(use-package-setup!)

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

;; default
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

;; async setup
(vim.schedule
  (fn []
    ;; modeline
    (let [modeline (require :dotfiles.modeline)]
      (modeline.setup))
    (vim.diagnostic.config {:virtual_text false})))

;; ;; lightline
;; (fn->viml :dotfiles.util :filename :LightlineFilename)
;; (fn->viml :dotfiles.util :readonly :LightlineReadonly)

;; (set nvim.g.lightline
;;      {:colorscheme :default
;;       :component_function {:filename :LightlineFilename
;;                            :readonly :LightlineReadonly}
;;       :active {:left [[:mode :paste]
;;                       [:readonly :filename :modified]]
;;                :right [[:lineinfo]
;;                        [:percent]]}
;;       :inactive {:left [[:filename]]
;;                  :right []}})
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

;; defer loading
(defer
  1
  (fn []
    (ex colorscheme :tokyonight-night)
    ;; (ex colorscheme :onedark)
    ;; (ex :Startify)

    ;; custom commands
    (fn->viml :dotfiles.util :gh-open-pull-request :GhOpenPullRequest)
    (fn->viml :dotfiles.util :gh-list-pull-requests :GhListPullRequests)
    (fn->viml :dotfiles.util :ci-open :CiOpen)
    (fn->viml :dotfiles.util :js-insert-i18n :JsInsertI18n)
    (fn->viml :dotfiles.util :org-roam-dailies-find-today :OrgRoamDailiesFindToday)
    (fn->viml :dotfiles.util :org-roam-dailies-find-yesterday :OrgRoamDailiesFindYesterday)
    (fn->viml :dotfiles.util :org-roam-dailies-find-tomorrow :OrgRoamDailiesFindTomorrow)
    (fn->viml :dotfiles.util :org-roam-find-file :OrgRoamFindFile)

    (noremap :n :gog "<cmd>call GhOpenPullRequest()<CR>" {:silent true})
    (noremap :n :goc "<cmd>call CiOpen()<CR>" {:silent true})
    (noremap :n :<f9> "<cmd>call OrgRoamFindFile()<CR>" {:silent true})

    (which-key.register
      {:/ ["<cmd>FzfLua live_grep<CR>" "Search project"]
       :* ["<cmd>FzfLua grep_cword<CR>" "Search at point"]
       :<tab> ["<C-^>" "Switch to last buffer"]
       :q {:name "+quit/session"
           :q ["<cmd>q<CR>" "Quit vim"]}
       :p {:name "+projects"
           :f ["<cmd>FzfLua files<CR>" "Find file"]
           :g ["<cmd>FzfLua tags<CR>" "Find Tags"]
           :a ["<cmd>:Other<CR>" "Toggle implementation and test"]}
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
           :oo ["<cmd>GBrowse<CR>" "Git browse"]
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
       :c {:name "+code"
           :x ["<cmd>lua vim.diagnostic.setqflist()<CR>" "Error List"]}
       :j {:name "+jump"
           :L ["<cmd>AnyJumpLastResults<CR>" "Jump to Last Results"]
           :j ["<cmd>HopChar1MW<CR>" "Jump to char"]
           :w ["<cmd>HopWordMW<CR>" "Jump to word"]
           :l ["<cmd>HopLine<CR>" "Jump to line"]}
       :r {:name "+registers"
           :e ["<cmd>FzfLua registers<CR>" "Registers"]}
       :t {:name "+toggle"
           :l ["<cmd>set nu! rnu!<CR>" "Toggle Line Number"]
           :i ["<cmd>IndentLinesToggle<CR>" "Toggle indent line"]}
       :n {:name "+notes"
           :r {:name "+roam"
               :f ["<cmd>call OrgRoamFindFile()<CR>" "org-roam-find-file"]
               :d {:name "+date"
                   :y ["<cmd>call OrgRoamDailiesFindYesterday()<CR>" "org-roam-dailies-find-yesterday"]
                   :m ["<cmd>call OrgRoamDailiesFindTomorrow()<CR>" "org-roam-dailies-find-tomorrow"]
                   :t ["<cmd>call OrgRoamDailiesFindToday()<CR>" "org-roam-dailies-find-today"]}}}
       :h {:name "+help"
           :? ["<cmd>FzfLua help_tags<CR>" "Help tags"]
           :e ["<cmd>messages<CR>" "View messages"]
           :df ["<cmd>FzfLua commands<CR>" "Help Commands"]
           :t ["<cmd>FzfLua colorschemes<CR>" "Load theme"]}}
      {:prefix "<leader>"})))

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

(noremap :n :<Leader>! "yy:let cliptext = getreg('*') | :call VimuxRunCommand(cliptext)<CR><CR>")
(noremap :v :<Leader>! "y:let cliptext = getreg('*') | :VimuxRunCommand(cliptext)<CR><CR>")

(noremap :n :<Leader>0 "<cmd>NvimTreeFindFile<CR>")
(noremap :n :<f5> ":TestNearest<CR>:TmuxNavigateDown<CR>")

(nmap :s "<cmd>HopChar1MW<CR>")
(nmap :gy "yygccp")

;; Diagnostic
(noremap :n "]e" "<cmd>lua vim.diagnostic.goto_next()<CR>")
(noremap :n "[e" "<cmd>lua vim.diagnostic.goto_prev()<CR>")

;; Jump
(noremap :n "g]" "<cmd>AnyJump<CR>")
(noremap :v "g]" "<cmd>AnyJumpVisual<CR>")
(noremap :n "g[" "<cmd>AnyJumpBack<CR>")

(augroup
  :MagitGit
  (autocmd :FileType :fugitive "nmap <buffer> q gq")
  (autocmd :FileType :fugitive "nmap <buffer> pp :Git push<CR>")
  (autocmd :FileType :fugitiveblame "nmap <buffer> q gq")

  (autocmd :FileType :NeogitRebaseTodo "imap <buffer> <C-c><C-c> <cmd>wq<CR>")
  (autocmd :FileType :NeogitRebaseTodo "nmap <buffer> <C-c><C-c> <cmd>wq<CR>")
  (autocmd :FileType :NeogitRebaseTodo "imap <buffer> <C-c><C-k> <cmd>q!<CR>")
  (autocmd :FileType :NeogitRebaseTodo "nmap <buffer> <C-c><C-k> <cmd>q!<CR>")

  (autocmd :FileType :NeogitCommitMessage "imap <buffer> <C-c><C-c> <cmd>wq<CR>")
  (autocmd :FileType :NeogitCommitMessage "nmap <buffer> <C-c><C-c> <cmd>wq<CR>")
  (autocmd :FileType :NeogitCommitMessage "imap <buffer> <C-c><C-k> <cmd>q!<CR>")
  (autocmd :FileType :NeogitCommitMessage "nmap <buffer> <C-c><C-k> <cmd>q!<CR>"))

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
  :Formatter
  (autocmd :BufWritePost "*.js,*.jsx,*.ts,*.tsx,*.rb" "FormatWrite"))

(augroup
  :Linter
  (autocmd :BufWritePost "*.js,*.jsx,*.ts,*.tsx,*.rb" "lua require('lint').try_lint()"))

;; not work
(inoremap :<C-Space> "compe#complete()" {:silent true :expr true})
(inoremap :<C-e> "compe#close('<C-e>')" {:silent true :expr true})
(inoremap :<C-f> "compe#scroll({ 'delta': +4 })" {:silent true :expr true})
(inoremap :<C-d> "compe#scroll({ 'delta': -4 })" {:silent true :expr true})

;; complete with auto-import
(inoremap :<CR> "compe#confirm({ 'keys': '<CR>', 'select': v:true })" {:expr true})
