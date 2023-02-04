local wk = require("which-key")

wk.register({
    p = { function () require('telescope').extensions.project.project() end, "Project" },
    f = {
        name = "+telescope",
        f = { function () require('telescope.builtin').find_files() end, "Files" },
        g = { function () require('telescope.builtin').live_grep() end, "Ripgrep" },
        b = { function () require('telescope.builtin').buffers() end, "Buffers" },
        h = { function () require('telescope.builtin').help_tags() end, "Help" }
    },
}, { prefix = "<leader>" })
