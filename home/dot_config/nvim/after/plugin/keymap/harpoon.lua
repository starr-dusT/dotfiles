local wk = require("which-key")

require("harpoon").setup({
    menu = {
        width = vim.api.nvim_win_get_width(0) - 4,
    }
})

wk.register({
    q = {
        name = "+harpoon",
        ["1"] = { function() require("harpoon.ui").nav_file(1) end, "Harpoon 1" },
        ["2"] = { function() require("harpoon.ui").nav_file(2) end, "Harpoon 2" },
        ["3"] = { function() require("harpoon.ui").nav_file(3) end, "Harpoon 3" },
        ["4"] = { function() require("harpoon.ui").nav_file(4) end, "Harpoon 4" },
        a = { function() require("harpoon.mark").add_file() end, "Add File" },
        l = { function() require("harpoon.ui").toggle_quick_menu() end, "List Files" }
    },
}, { prefix = "<leader>" })
