local utils = require('utils')
local opt = utils.opt

local cmd = vim.cmd
local indent = 4

cmd 'syntax enable'
cmd 'filetype plugin indent on'

opt('o', 'termguicolors', true)
cmd 'colorscheme doom-one'
