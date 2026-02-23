-- ~/.config/nvim/init.lua

-- Section 1: First-Time Bootstrap for the nfnl Compiler
local main_config_file = vim.fn.stdpath("config") .. "/lua/dotfiles/init.lua"

if not vim.loop.fs_stat(main_config_file) then
  print("nfnl: No compiled config found. Bootstrapping compiler...")

  -- Manually install nfnl to a bootstrap location.
  local bootstrap_dir_root = vim.fn.stdpath("data") .. "/site/pack/bootstrap"
  local nfnl_dir = bootstrap_dir_root .. "/start/nfnl.vim"
  local bootstrap_start_dir = bootstrap_dir_root .. "/start"

  if not vim.loop.fs_stat(nfnl_dir) then
    print("nfnl: Cloning compiler...")
    vim.fn.system({
      "git", "clone", "--depth=1",
      "https://github.com/Olical/nfnl.git", nfnl_dir,
    })
  end

  -- Add the bootstrap package to the runtime path and load it.
  vim.opt.rtp:prepend(bootstrap_start_dir)
  vim.cmd("packloadall!")

  print("nfnl: Compiling configuration...")
  local ok, nfnl_api = pcall(require, "nfnl.api")
  if not ok then
    error("FATAL: Failed to require 'nfnl.api' even after manual bootstrap. Error: " .. tostring(nfnl_api))
  end

  -- === THE FIX ===
  -- Temporarily change to the config directory so nfnl can find its config file.
  local config_dir = vim.fn.stdpath("config")
  local original_dir = vim.fn.getcwd()
  vim.cmd.cd(config_dir)

  -- Now, compile. This will find the .nfnl.fnl file in the new current directory.
  nfnl_api["compile-all-files"]()

  -- Change back to the original directory to avoid side-effects.
  vim.cmd.cd(original_dir)
  -- ===============

  print("nfnl: Compilation complete! Please restart Neovim now.")
end

-- Section 2: Normal Startup with lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/site/pack/bootstrap/start/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("dotfiles.init")
