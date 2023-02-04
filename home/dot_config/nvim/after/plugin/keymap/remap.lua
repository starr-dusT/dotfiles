local wk = require("which-key")

wk.register({
    -- netrw commands
    n = {
        name = "+netrw",
        v = { "<cmd>Ex<CR>", "Explorer" }
    },
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
