(module dotfiles.config
  {autoload {nvim aniseed.nvim}
   require-macros [dotfiles.macros]})

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

(require :dotfiles.modules.javascript)
