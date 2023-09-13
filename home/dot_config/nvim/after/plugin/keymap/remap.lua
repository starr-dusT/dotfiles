local wk = require("which-key")

-- Move visual blocks up and down
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Keep cursor in center while jumping half-pages
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Keep cursor in center while searching
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- Change clipboard to clip to
vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])

-- Quick fix controls
vim.keymap.set("n", "<C-k>", "<cmd>cnext<CR>zz")
vim.keymap.set("n", "<C-j>", "<cmd>cprev<CR>zz")
vim.keymap.set("n", "<leader>k", "<cmd>lnext<CR>zz")
vim.keymap.set("n", "<leader>j", "<cmd>lprev<CR>zz")

-- Replace word under cursor
vim.keymap.set("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])

wk.register({
    -- tab commands
    t = {
        name = "+tab",
        f = { "<cmd>tabnew<CR>", "New" },
        g = { "<cmd>tabn<CR>", "Next" },
        b = { "<cmd>tabp<CR>","Previous" },
        h = { "<cmd>tabc<CR>", "Close" }
    },
    -- pane movement commands
    h = { "<C-w>h", "Pane Left" },
    j = { "<C-w>j", "Pane Down" },
    k = { "<C-w>k", "Pane Up" },
    l = { "<C-w>l", "Pane Right" }
}, { prefix = "<leader>" })
