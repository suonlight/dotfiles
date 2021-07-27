(module dotfiles.packages
  {autoload {nvim aniseed.nvim
             a aniseed.core
             util dotfiles.util
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
  ; "~/repos/Olical/conjure" {:mod :conjure}
  ; "~/repos/Olical/aniseed" {}
  ; "~/repos/Olical/nvim-local-fennel" {}

  ; :HerringtonDarkholme/yats.vim {}
  ; :LnL7/vim-nix {}
  ; :Olical/AnsiEsc {}
  :Olical/aniseed {}
  :Olical/nvim-local-fennel {}
  :Olical/conjure {:mod :conjure}

  :guns/vim-sexp {:mod :sexp}
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
  :folke/which-key.nvim {:mod :which-key}
  :nvim-lua/popup.nvim {}
  :nvim-lua/plenary.nvim {}
  :nvim-telescope/telescope.nvim {:mod :telescope}
  :mhinz/vim-startify {}

  ; text objects
  :kana/vim-textobj-user {}
  :kana/vim-textobj-indent  {}
  :kana/vim-textobj-line  {}
  :kana/vim-textobj-entire  {:mod textobject-entire}

  ; ui
  :romgrk/doom-one.vim {}
  :kyazdani42/nvim-web-devicons {}
  :itchyny/lightline.vim {:mod :lightline}
  ; :glepnir/galaxyline.nvim {:branch :main}

  ; linter
  :dense-analysis/ale {:mod :ale}

  ; javascript
  :prettier/vim-prettier {:mod :prettier}

  ; ruby
  :tpope/vim-projectionist {:mod :projectionist}

  ; lsp
  :neovim/nvim-lspconfig {}

  ; notes
  :kristijanhusak/orgmode.nvim {:mod :orgmode}

  ; :Olical/vim-enmasse {}
  ; :PeterRincker/vim-argumentative {}
  ; :airblade/vim-gitgutter {}
  ; :clojure-vim/clojure.vim {}
  ; :clojure-vim/vim-jack-in {}
  ; :dag/vim-fish {}
  ; :easymotion/vim-easymotion {:mod :easymotion}
  ; :guns/vim-sexp {:mod :sexp}
  ; :hashivim/vim-terraform {}
  ; :hrsh7th/nvim-compe {:mod :compe}
  ; :hylang/vim-hy {}
  ; :janet-lang/janet.vim {}
  :jiangmiao/auto-pairs {:mod :auto-pairs}
  ; :junegunn/fzf {:mod :fzf}
  ; :junegunn/fzf.vim {}
  ; :lambdalisue/suda.vim {}
  ; :liuchengxu/vim-better-default {:mod :better-default}
  ; :maxmellon/vim-jsx-pretty {}
  ; :mbbill/undotree {:mod :undotree}
  ; :norcalli/nvim-colorizer.lua {:mod :colorizer}
  ; :pangloss/vim-javascript {}
  ; :radenling/vim-dispatch-neovim {}
  ; :srcery-colors/srcery-vim {:mod :srcery}
  ; :tami5/compe-conjure {}
  ; :tpope/vim-abolish {}
  ; :tpope/vim-commentary {}
  ; :tpope/vim-dadbod {}
  ; :tpope/vim-dispatch {}
  ; :tpope/vim-eunuch {}
  ; :tpope/vim-fugitive {:mod :fugitive}
  ; :tpope/vim-repeat {}
  ; :tpope/vim-sexp-mappings-for-regular-people {}
  ; :tpope/vim-sleuth {}
  ; :tpope/vim-surround {}
  ; :tpope/vim-unimpaired {}
  ; :tpope/vim-vinegar {}
  ; :tweekmonster/startuptime.vim {}
  ; :wbthomason/packer.nvim {}
  ; :wlangstroth/vim-racket {}
  )
