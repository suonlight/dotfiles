(module dotfiles.bindings
  {autoload {nvim aniseed.nvim
             which-key which-key}
   require-macros [dotfiles.macros]})

(which-key.register
  {:/ ["<cmd>Telescope live_grep<CR>" "Search project"]
   :* ["<cmd>Telescope grep_string<CR>" "Search at point"]
   :<tab> ["<C-^>" "Switch to last buffer"]
   :q {:name "+quit/session"
       :q ["<cmd>q<CR>" "Quit vim"]}
   :p {:name "+projects"
       :f ["<cmd>Telescope find_files<CR>" "Find file"]
       :a ["<cmd>A<CR>" "Toggle implementation and test"]}
   :f {:name "+files"
       :s ["<cmd>update<CR>" "File save"]
       :r ["<cmd>Telescope oldfiles<CR>" "Recent files"]}
   :g {:name "+git"
       :s ["<cmd>Git<CR>" "Git status"]
       :b ["<cmd>Git blame<CR>" "Git blame"]}
   :b {:name "+buffers"
       :b ["<cmd>Telescope buffers<CR>" "Find buffer"]
       :d ["<cmd>bdelete<CR>" "Delete buffer"]}
   :w {:name "+windows"
       :h ["<cmd>wincmd h<CR>" "Window left"]
       :l ["<cmd>wincmd l<CR>" "Window right"]
       :j ["<cmd>wincmd j<CR>" "Window down"]
       :k ["<cmd>wincmd k<CR>" "Window up"]
       :w ["<cmd>wincmd w<CR>" "Other window"]
       := ["<cmd>wincmd =<CR>" "Window balance area"]
       :r ["<cmd>wincmd r<CR>" "Rotate window"]
       :s ["<cmd>wincmd s<CR>" "Window split"]
       :v ["<cmd>wincmd v<CR>" "Window vsplit"]
       :c ["<cmd>close<CR>" "Window close"]}
   :r {:name "+registers"
       :e ["<cmd>Telescope registers<CR>" "Registers"]}
   :h {:name "+help"
       :? ["<cmd>Telescope help_tags<CR>" "Help tags"]
       :e ["<cmd>messages<CR>" "View messages"]
       :df ["<cmd>Telescope commands<CR>" "Help Commands"]
       :t ["<cmd>Telescope colorscheme<CR>" "Load theme"]}}
  {:prefix "<leader>"})

;; Generic mapping configuration.
(set nvim.g.mapleader " ")
(set nvim.g.maplocalleader ",")

(noremap :n :<space> :<nop>)
(noremap :n :<M-s> "<cmd>update<CR>")
(noremap :n :<M-b> "<cmd>Telescope buffers<CR>")
(noremap :n :<M-w> "<cmd>close<CR>")
(noremap :n :<C-p> "<cmd>Telescope find_files<CR>")
(noremap :n :<C-h> "<cmd>wincmd h<CR>")
(noremap :n :<C-l> "<cmd>wincmd l<CR>")
(noremap :n :<C-j> "<cmd>wincmd j<CR>")
(noremap :n :<C-k> "<cmd>wincmd k<CR>")

(nmap :gy "<cmd>yank | Commentary<CR>p")

(defer 100
  (fn []
    (autocmd :FileType :fugitive "nmap <buffer> q gq")
    (autocmd :FileType :fugitive "nmap <buffer> pp :Git push<CR>")
    (autocmd :FileType :fugitiveblame "nmap <buffer> q gq")
    (autocmd :FileType :gitcommit "nmap <buffer> <C-c><C-c> :wq<CR>")
    (autocmd :FileType :gitcommit "nmap <buffer> <C-c><C-k> :q!<CR>")))
