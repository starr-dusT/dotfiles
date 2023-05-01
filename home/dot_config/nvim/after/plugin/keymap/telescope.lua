local wk = require("which-key")

wk.register({
    p = { function () require('telescope').extensions.project.project() end, "Project" },
    b = { function () require('telescope.builtin').buffers() end, "Buffers" },
    f = {
        name = "+telescope",
        f = { function () require('telescope.builtin').find_files() end, "Files" },
        g = { function () require('telescope.builtin').live_grep() end, "Ripgrep" },
        h = { function () require('telescope.builtin').help_tags() end, "Help" }
    },
}, { prefix = "<leader>" })
