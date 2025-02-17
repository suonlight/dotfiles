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

(use-package! :folke/which-key.nvim
              :event "VeryLazy"
              :config
              (fn []
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

                (which-key.add
                  [{1 "<leader>/" 2 "<cmd>FzfLua live_grep<CR>" :desc "Search project"}
                   {1 "<leader>*" 2 "<cmd>FzfLua grep_cword<CR>" :desc "Search at point"}
                   {1 "<leader><tab>" 2 "<C-^>" :desc "Switch to last buffer"}
                   {1 "<leader>q" :group "+quit/session"}
                   {1 "<leader>qq" 2 "<cmd>q<CR>" :desc "Quit vim"}
                   {1 "<leader>p" :group "+projects"}
                   {1 "<leader>pf" 2 "<cmd>FzfLua files<CR>" :desc "Find file"}
                   {1 "<leader>pg" 2 "<cmd>FzfLua tags<CR>" :desc "Find Tags"}
                   {1 "<leader>pa" 2 "<cmd>:Other<CR>" :desc "Toggle implementation and test"}
                   {1 "<leader>f" :group "+files"}
                   {1 "<leader>fs" 2 "<cmd>update<CR>" :desc "File save"}
                   {1 "<leader>ff" 2 "<cmd>NvimTreeFindFile<CR>" :desc "Find file in directory"}
                   {1 "<leader>ft" 2 "<cmd>NvimTreeToggle<CR>" :desc "Toggle Tree"}
                   {1 "<leader>fR" 2 ":Rename " :desc "Rename file"}
                   {1 "<leader>fc" 2 ":saveas <C-R>=expand(\"%:p:h\")<CR>/" :desc "Copy file"}
                   {1 "<leader>fr" 2 "<cmd>FzfLua oldfiles<CR>" :desc "Recent files"}
                   {1 "<leader>fy" 2 ":let @*=expand('%:p') | echo @*<CR>" :desc "Copy Full File Path"}
                   {1 "<leader>g" :group "+git"}
                   {1 "<leader>gp" 2 "<cmd>call GhListPullRequests()<CR>" :desc "Github List PRs"}
                   {1 "<leader>goo" 2 "<cmd>GBrowse<CR>" :desc "Git browse"}
                   {1 "<leader>gs" 2 "<cmd>Neogit<CR>" :desc "Git status"}
                   {1 "<leader>gb" 2 "<cmd>Git blame<CR>" :desc "Git blame"}
                   {1 "<leader>b" :group "+buffers"}
                   {1 "<leader>bb" 2 "<cmd>FzfLua buffers<CR>" :desc "Find buffer"}
                   {1 "<leader>bd" 2 "<cmd>bdelete<CR>" :desc "Delete buffer"}
                   {1 "<leader>bn" 2 "<cmd>bdelete<CR>" :desc "Next buffer"}
                   {1 "<leader>bp" 2 "<cmd>bdelete<CR>" :desc "Previous buffer"}
                   {1 "<leader>bh" 2 "<cmd>Startify<CR>" :desc "Home buffer"}
                   {1 "<leader>w" :group "+windows"}
                   {1 "<leader>wh" 2 "<cmd>wincmd h<CR>" :desc "Window left"}
                   {1 "<leader>wl" 2 "<cmd>wincmd l<CR>" :desc "Window right"}
                   {1 "<leader>wj" 2 "<cmd>wincmd j<CR>" :desc "Window down"}
                   {1 "<leader>wk" 2 "<cmd>wincmd k<CR>" :desc "Window up"}
                   {1 "<leader>w<S-h>" 2 "<cmd>wincmd <S-h><CR>" :desc "Move window far left"}
                   {1 "<leader>w<S-l>" 2 "<cmd>wincmd <S-l><CR>" :desc "Move window far right"}
                   {1 "<leader>w<S-j>" 2 "<cmd>wincmd <S-j><CR>" :desc "Move window very down"}
                   {1 "<leader>w<S-k>" 2 "<cmd>wincmd <S-k><CR>" :desc "Move window very top"}
                   {1 "<leader>ww" 2 "<cmd>wincmd w<CR>" :desc "Other window"}
                   {1 "<leader>w=" 2 "<cmd>wincmd =<CR>" :desc "Window balance area"}
                   {1 "<leader>wr" 2 "<cmd>wincmd r<CR>" :desc "Rotate window"}
                   {1 "<leader>ws" 2 "<cmd>wincmd s<CR>" :desc "Window split"}
                   {1 "<leader>wv" 2 "<cmd>wincmd v<CR>" :desc "Window vsplit"}
                   {1 "<leader>wc" 2 "<cmd>close<CR>" :desc "Window close"}
                   {1 "<leader>s" :group "+search"}
                   {1 "<leader>sp" 2 "<cmd>FzfLua live_grep<CR>" :desc "Search in project"}
                   {1 "<leader>ss" 2 "<cmd>FzfLua grep_curbuf<CR>" :desc "Search in buffer"}
                   {1 "<leader>c" :group "+code"}
                   {1 "<leader>cx" 2 "<cmd>lua vim.diagnostic.setqflist()<CR>" :desc "Error List"}
                   {1 "<leader>j" :group "+jump"}
                   {1 "<leader>jL" 2 "<cmd>AnyJumpLastResults<CR>" :desc "Jump to Last Results"}
                   {1 "<leader>jj" 2 "<cmd>HopChar1MW<CR>" :desc "Jump to char"}
                   {1 "<leader>jw" 2 "<cmd>HopWordMW<CR>" :desc "Jump to word"}
                   {1 "<leader>jl" 2 "<cmd>HopLine<CR>" :desc "Jump to line"}
                   {1 "<leader>r" :group "+registers"}
                   {1 "<leader>re" 2 "<cmd>FzfLua registers<CR>" :desc "Registers"}
                   {1 "<leader>t" :group "+toggle"}
                   {1 "<leader>tl" 2 "<cmd>set nu! rnu!<CR>" :desc "Toggle Line Number"}
                   {1 "<leader>ti" 2 "<cmd>IndentLinesToggle<CR>" :desc "Toggle indent line"}
                   {1 "<leader>n" :group "+notes"}
                   {1 "<leader>nr" :group "+roam"}
                   {1 "<leader>nrS" 2 "<cmd>:RoamReset true<CR>" :desc "Sync Database"}
                   {1 "<leader>nrf" 2 "<cmd>lua require('org-roam').api.find_node()<CR>" :desc "org-roam-find-file"}
                   {1 "<leader>nri" 2 "<cmd>lua require('org-roam').api.insert_node()<CR>" :desc "org-roam-insert-node"}
                   {1 "<leader>nrd" :group "+date"}
                   {1 "<leader>nrdy" 2 "<cmd>call OrgRoamDailiesFindYesterday()<CR>" :desc "org-roam-dailies-find-yesterday"}
                   {1 "<leader>nrdm" 2 "<cmd>call OrgRoamDailiesFindTomorrow()<CR>" :desc "org-roam-dailies-find-tomorrow"}
                   {1 "<leader>nrdt" 2 "<cmd>call OrgRoamDailiesFindToday()<CR>" :desc "org-roam-dailies-find-today"}
                   {1 "<leader>h" :group "+help"}
                   {1 "<leader>h?" 2 "<cmd>FzfLua help_tags<CR>" :desc "Help tags"}
                   {1 "<leader>he" 2 "<cmd>messages<CR>" :desc "View messages"}
                   {1 "<leader>hdf" 2 "<cmd>FzfLua commands<CR>" :desc "Help Commands"}
                   {1 "<leader>ht" 2 "<cmd>FzfLua colorschemes<CR>" :desc "Load theme"}])))

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

(use-package! :stevearc/dressing.nvim
              :enabled true
              :config
              (fn []
                (let [dressing (require :dressing)]
                  (dressing.setup
                    {:input {:enabled true
                             :default_prompt "Input:"
                             :title_pos "center"
                             :insert_only true
                             :start_in_insert true
                             ;; :anchor "SW"
                             :border "rounded"
                             :relative "cursor"
                             :prefer_width 40
                             :width nil
                             :max_width [140 0.9]
                             :min_width [20 0.2]
                             :buf_options {}
                             :win_options {:winblend 10
                                           :wrap false
                                           :list true
                                           :listchars "precedes:…,extends:…"
                                           :sidescrolloff 0}
                             :mappings {:n {"<Esc>" "Close"
                                            "<CR>" "Confirm"}
                                        :i {"<C-c>" "Close"
                                            "<CR>" "Confirm"
                                            "<Up>" "HistoryPrev"
                                            "<Down>" "HistoryNext"}}
                             :override (fn [conf]
                                         ;; This is the config that will be passed to nvim_open_win.
                                         ;; Change values here to customize the layout
                                         conf)
                             ;; :see :help dressing_get_config
                             :get_config nil}
                     :select {:enabled true
                              :backend ["telescope" "fzf_lua" "fzf" "builtin" "nui"]
                              :trim-prompt true
                              :telescope nil
                              :fzf {:window {:width 0.5 :height 0.4}}
                              :fzf-lua {}
                              :nui {:position "50%"
                                    :relative "editor"
                                    :border {:style "rounded"}
                                    :buf-options {:swapfile false :filetype "DressingSelect"}
                                    :win-options {:winblend 10}
                                    :max-width 80
                                    :max-height 40
                                    :min-width 40
                                    :min-height 10}
                              :builtin {:override "NW"
                                        :border "rounded"
                                        :relative "editor"
                                        :buf-options {}
                                        :win-options {:winblend 10}
                                        :max-width [140 0.8]
                                        :min-width [40 0.2]
                                        :max-height 0.9
                                        :min-height [10 0.2]
                                        :mappings {["<Esc>"] "Close"
                                                   ["<C-c>"] "Close"
                                                   ["<CR>"] "Confirm"}
                                        :override (fn [conf] conf)}
                              :format-item-override {}
                              :get-config nil}}))))

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
                  (fzf-lua.setup {})
                  ; (fzf-lua.setup {:winopts {:split "belowright new"}})
                  )))

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
(use-package! :echasnovski/mini.icons )
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
                  (lsp.ts_ls.setup {:on_attach on-attach}))))

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
(use-package! :mfussenegger/nvim-lint :event "VeryLazy")
(use-package! :github/copilot.vim :event "VeryLazy")
(use-package! :CopilotC-Nvim/CopilotChat.nvim
              :event "VeryLazy"
              :dependencies ["github/copilot.vim" "nvim-lua/plenary.nvim"]
              :opts {:debug true}
              :config
              (fn []
                (let [copilot-chat (require :CopilotChat)]
                  (copilot-chat.setup
                    {:debug true
                     :prompts {:TextWording {:prompt "Please improve the grammar and wording of the following text."}
                               :TextConcise { :prompt "Please rewrite the following text to make it more concise." }}}))

                (noremap [:n :v] :<Leader>cch
                           (fn []
                               (let [actions (require :CopilotChat.actions)
                                     fzf-lua-int (require :CopilotChat.integrations.fzflua)]
                                 (fzf-lua-int.pick (actions.help_actions))))
                           {:desc "Help actions"})
                (noremap [:n :v] :<Leader>ccp
                         (fn []
                           (let [actions (require :CopilotChat.actions)
                                 fzf-lua-int (require :CopilotChat.integrations.fzflua)]
                             (fzf-lua-int.pick (actions.prompt_actions))))
                         {:desc "Prompt actions"})
                (noremap [:n :v] :<Leader>ccq
                         (fn []
                           (vim.ui.input {:prompt "Quick Chat: "}
                                         (fn [input]
                                           (when (not (= nil input))
                                             (vim.cmd (.. "CopilotChat " input))))))
                         {:desc "Quick chat"})
                (noremap [:n :v] :<Leader>ccc
                         "<cmd>CopilotChatToggle<CR>"
                         {:desc "Toggle Chat"})
                (noremap [:n :v] :<Leader>cco
                         "<cmd>CopilotChat<CR>"
                         {:desc "Open Chat"})))

; notes
(use-package! :nvim-orgmode/orgmode
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
                                  :ensure_installed ["org" "markdown" "diff"]})
                  (sniprun.setup {:display ["Classic" "NvimNotify"]
                                  :display_options {:notification_timeout 10}})
                  (orgmode.setup {:org_todo_keywords ["TODO" "DOING" "|" "DONE"]
                                  :mappings {:org {:org_todo "t"}}}))))

(use-package! :akinsho/org-bullets.nvim
              :dependencies [:nvim-orgmode/orgmode]
              :config
              (fn []
                (let [orgbullets (require :org-bullets)]
                  (orgbullets.setup
                   {:concealcursor true
                    :symbols {:headlines ["◉" "○" "✸" "✿"]}}))))

; (use-package! :chipsenkbeil/org-roam.nvim
;               :dependencies [:nvim-orgmode/orgmode]
;               :config
;               (fn []
;                 (let [org-roam (require :org-roam)]
;                     (org-roam.setup {:directory "~/notes/roam"
;                                      :database {:path "~/.local/share/nvim/org-roam/db"}
;                                      :extensions {:dailies {:directory "journals"}}}))))

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

    ))

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
