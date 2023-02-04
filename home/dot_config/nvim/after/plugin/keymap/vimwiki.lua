local wk = require("which-key")

wk.register({
    w = {
        name = "+vimwiki",
        i = { "<cmd>VimwikiDiaryIndex<CR>", "Diary Index" },
        s = { "<cmd>VimwikiUISelect<CR>", "Select Index" },
        t = { "<cmd>VimwikiTabIndex<CR>", "Index Tab" },
        w = { "<cmd>VimwikiIndex<CR>", "Index" },
        ["<space>"] = {
            name = "+diary",
            i = { "<cmd>VimwikiDiaryGenerateLinks<CR>", "Generate Links" },
            m = { "<cmd>VimwikiMakeTomorrowDiaryNote<CR>", "Make Tomorrow Note" },
            t = { "<cmd>VimwikiTabMakeDiaryNote<CR>", "Make Today Note" },
            w = { "<cmd>VimwikiDiaryNote<CR>", "Today Note" },
            y = { "<cmd>VimwikiMakeYesterdayDiaryNote<CR>", "Make Yesterday Note" }
        }
    },
}, { prefix = "<leader>" })
