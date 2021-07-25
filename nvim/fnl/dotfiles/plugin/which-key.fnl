(module dotfiles.plugin.which-key
  {autoload {nvim aniseed.nvim
             which-key which-key}})

; (which-key.register 
;   {:p {:name "+projects"
;        :f ["<cmd>Telescope find_files<CR>" "Find File"]}
;    :g {:name "+git"
;        :s ["<cmd>Git<CR>" "Git Status"]}
;    :b {:name "+buffers"
;        :b ["<cmd>Telescope buffers<CR>" "Find Buffer"]}
;    :w {:name "+windows"
;        :h ["<cmd>wincmd h<CR>" "Window Left"]
;        :l ["<cmd>wincmd l<CR>" "Window Right"]
;        :j ["<cmd>wincmd j<CR>" "Window Down"]
;        :k ["<cmd>wincmd k<CR>" "Window Up"]
;        :w ["<cmd>wincmd w<CR>" "Other Window"]
;        := ["<cmd>wincmd =<CR>" "Window Balance Area"]
;        :r ["<cmd>wincmd r<CR>" "Rotate Window"]
;        :s ["<cmd>wincmd s<CR>" "Window Split"]
;        :v ["<cmd>wincmd v<CR>" "Window VSplit"]
;        :c ["<cmd>close<CR>" "Window close"]}} 
;   {:prefix "<leader>"})
