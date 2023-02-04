local wk = require("which-key")

wk.register({
    g = {
        name = "+git",
        g = { "<cmd>LazyGit<CR>", "Lazygit" },
    },
}, { prefix = "<leader>" })
