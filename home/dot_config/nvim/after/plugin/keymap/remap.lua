-- Set leader to <space>
vim.g.mapleader = " "

-- Native lsp keybinds
vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format, { desc = "Format buffer" })
