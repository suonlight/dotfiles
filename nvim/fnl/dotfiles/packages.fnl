(module dotfiles.packages
  {autoload {nvim aniseed.nvim
             a aniseed.core
             packer packer}})

(defn safe-require-plugin-config [name]
  (let [(ok? val-or-err) (pcall require (.. :dotfiles.plugin. name))]
    (when (not ok?)
      (print (.. "dotfiles error: " val-or-err)))))

(defn- use [...]
  "Iterates through the arguments as pairs and calls packer's use function for
  each of them. Works around Fennel not liking mixed associative and sequential
  tables as well."
  (let [pkgs [...]]
    (packer.startup
      (fn [use]
        (for [i 1 (a.count pkgs) 2]
          (let [name (. pkgs i)
                opts (. pkgs (+ i 1))]
            (-?> (. opts :mod) (safe-require-plugin-config))
            (use (a.assoc opts 1 name))))))))

;; Plugins to be managed by packer.
(use
  :Olical/aniseed {}
  :Olical/nvim-local-fennel {}
  :Olical/conjure {}

  :guns/vim-sexp {}
  :tpope/vim-surround {}
  :tpope/vim-commentary {}
  :tpope/vim-fugitive {}

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

  ; defaults
  :editorconfig/editorconfig-vim {}
  :folke/which-key.nvim {}
  :nvim-lua/popup.nvim {}
  :nvim-lua/plenary.nvim {}
  :nvim-telescope/telescope.nvim {}
  :mhinz/vim-startify {}
  :jiangmiao/auto-pairs {}

  ; text objects
  :kana/vim-textobj-user {}
  :kana/vim-textobj-indent  {}
  :kana/vim-textobj-line  {}
  :kana/vim-textobj-entire  {}

  ; ui
  :romgrk/doom-one.vim {}
  :kyazdani42/nvim-web-devicons {}
  :itchyny/lightline.vim {}
  ; :glepnir/galaxyline.nvim {:branch :main}

  ; linter
  :dense-analysis/ale {}

  ; javascript
  :prettier/vim-prettier {}
  :pangloss/vim-javascript {}
  :maxmellon/vim-jsx-pretty {}

  ; ruby
  :tpope/vim-projectionist {}

  ; lsp
  :neovim/nvim-lspconfig {}

  ; notes
  :kristijanhusak/orgmode.nvim {}

  ; :Olical/vim-enmasse {}
  ; :PeterRincker/vim-argumentative {}
  ; :airblade/vim-gitgutter {}
  ; :clojure-vim/clojure.vim {}
  ; :clojure-vim/vim-jack-in {}
  ; :dag/vim-fish {}
  ; :easymotion/vim-easymotion {:mod :easymotion}
  ; :hrsh7th/nvim-compe {:mod :compe}
  ; :hylang/vim-hy {}
  ; :lambdalisue/suda.vim {}
  ; :liuchengxu/vim-better-default {:mod :better-default}
  ; :mbbill/undotree {:mod :undotree}
  ; :norcalli/nvim-colorizer.lua {:mod :colorizer}
  ; :radenling/vim-dispatch-neovim {}
  ; :tweekmonster/startuptime.vim {}
  ; :wlangstroth/vim-racket {}
  )
