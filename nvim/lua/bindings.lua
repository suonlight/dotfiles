local utils = require('utils')
local map = utils.map

vim.g.mapleader = ' '

map("n", "<Leader>/", "<cmd>Telescope live_grep<CR>")
map("n", "<Leader>qq", "<cmd>q<CR>")

-- Files
map("n", "<Leader>fs", "<cmd>update<CR>")

-- Projects
map("n", "<Leader>pf", "<cmd>Telescope find_files<CR>")

-- Buffers
map("n", "<Leader>bb", "<cmd>Telescope buffers<CR>")

-- Global
map("n", "<C-p>", "<cmd>Telescope find_files<CR>")
