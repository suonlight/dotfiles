local utils = require('utils')

vim.g.mapleader = ' '

utils.map("n", "<Leader>/", "<cmd>Rg<CR>")
utils.map("n", "<Leader>qq", "<cmd>q<CR>")

-- Files
utils.map("n", "<Leader>fs", "<cmd>update<CR>")

-- Projects
utils.map("n", "<Leader>pf", "<cmd>Telescope find_files<CR>")

-- Buffers
utils.map("n", "<Leader>bb", "<cmd>Telescope buffers<CR>")
