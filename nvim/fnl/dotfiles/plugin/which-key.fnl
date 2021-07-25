(module dotfiles.plugin.which-key
  {autoload {nvim aniseed.nvim
             which-key which-key}})

(which-key.register 
  {:p {:name "Projects"
       :f ["<cmd>Telescope find_files<CR>" "Find File"]}
   :g {:name "Git"
       :s ["<cmd>Git<CR>" "Git Status"]}} 
  {:prefix "<leader>"})
