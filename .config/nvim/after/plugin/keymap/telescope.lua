local Remap = require("tstarr.keymap")
local nnoremap = Remap.nnoremap

nnoremap("<leader>ff", function ()
    require('telescope.builtin').find_files({hidden=true, no_ignore=true})
end)
nnoremap("<leader>fg", function ()
    require('telescope.builtin').live_grep()
end)
nnoremap("<leader>fb", function ()
    require('telescope.builtin').buffers()
end)
nnoremap("<leader>fh", function ()
    require('telescope.builtin').help_tags()
end)

nnoremap("<leader>p", function ()
    require('telescope').extensions.project.project()
end)
