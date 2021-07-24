(module dotfiles.bindings
  {autoload {nvim aniseed.nvim
             nu aniseed.nvim.util
             core aniseed.core}})

(defn- noremap [mode from to]
  "Sets a mapping with {:noremap true}."
  (nvim.set_keymap mode from to {:noremap true}))

;; Generic mapping configuration.
(nvim.set_keymap :n :<space> :<nop> {:noremap true})
(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

;; Projects
(noremap :n :<leader>pf ":Telescope find_files<CR>")
(noremap :n :<C-p> ":Telescope find_files<CR>")

;; Files
(noremap :n :<leader>fs ":update<CR>")

;; General
(noremap :n :<M-s> ":update<CR>")
(noremap :n :<leader>qq ":q<CR>")

