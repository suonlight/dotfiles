(module dotfiles.config
  {autoload {nvim aniseed.nvim}
   require-macros [dotfiles.macros]})

(_: colorscheme :doom-one)

(set nvim.o.termguicolors true)
(set nvim.o.clipboard "unnamed")
