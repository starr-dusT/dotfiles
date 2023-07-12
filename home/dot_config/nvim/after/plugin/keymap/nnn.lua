local wk = require("which-key")

wk.register({
    -- nnn and netrw commands
    n = {
        name = "+nnn",
        n = { "<cmd>NnnPicker<CR>", "NNN Picker" }, 
        e = { "<cmd>NnnExplorer<CR>", "NNN Explorer" }, 
        v = { "<cmd>Ex<CR>", "Netrw Explorer" }
    },
}, { prefix = "<leader>" })
