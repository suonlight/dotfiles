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

(noremap :n :<M-s> ":update<CR>")
(noremap :n :<M-b> ":Telescope buffers<CR>")
(noremap :n :<leader>qq ":q<CR>")
(noremap :n :<leader><tab> "<C-6>")
(noremap :n :<leader>/ ":Telescope live_grep<CR>")

;; Projects
;; (noremap :n :<leader>pf ":Telescope find_files<CR>")
(noremap :n :<C-p> ":Telescope find_files<CR>")

;; Files
(noremap :n :<leader>fs ":update<CR>")

