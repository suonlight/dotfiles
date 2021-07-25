(module dotfiles.plugin.telescope
  {autoload {nvim aniseed.nvim
             telescope telescope
             actions telescope.actions}})

(telescope.setup 
  {:defaults {:mappings 
              {:i {"<esc>" actions.close}}}})

