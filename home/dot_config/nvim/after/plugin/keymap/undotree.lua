local wk = require("which-key")

wk.register({
    q = {
        name = "+undotree",
        u = { "<cmd>UndotreeFocus<CR>", "Focus Tree" },
        t = { "<cmd>UndotreeToggle<CR>", "Toggle Tree" }
    },
}, { prefix = "<leader>" })
