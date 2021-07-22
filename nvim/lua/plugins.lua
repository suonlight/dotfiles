return require('packer').startup(function()

  -- Packer can manage itself as an optional plugin
  use {'wbthomason/packer.nvim', opt = true}

  -- Color scheme
  use { 'romgrk/doom-one.vim' }

  -- Fuzzy finder
  use {
      'nvim-telescope/telescope.nvim',
      requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
  }

  use {
    'kristijanhusak/orgmode.nvim', config = function()
      require('orgmode').setup{}
    end
  }

  -- -- LSP and completion
  use { 'neovim/nvim-lspconfig' }
  use { 'nvim-lua/completion-nvim' }

  -- -- Lua development
  -- use { 'tjdevries/nlua.nvim' }


  -- -- Vim dispatch
  -- use { 'tpope/vim-dispatch' }

  -- -- Fugitive for Git
  -- use { 'tpope/vim-fugitive' }

end)
