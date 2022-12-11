local Remap = require("tstarr.keymap")
local nnoremap = Remap.nnoremap
local silent = { silent = true }

nnoremap("<leader>gg", "<cmd>LazyGit<CR>", silent)
