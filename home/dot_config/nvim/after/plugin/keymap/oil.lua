local wk = require("which-key")

require("oil").setup()

wk.register({
    o = {
        name = "+oil",
        o = { function() require("oil").open() end, "Open Oil" }
    },
}, { prefix = "<leader>" })
