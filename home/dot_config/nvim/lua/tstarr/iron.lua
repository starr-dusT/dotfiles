local iron = require("iron.core")
local wk = require("which-key")

iron.setup {
  config = {
    -- Whether a repl should be discarded or not
    scratch_repl = true,
    -- Your repl definitions come here
    repl_definition = {
      sh = {
        -- Can be a table or a function that
        -- returns a table (see below)
        command = {"zsh"}
      },
      python = require("iron.fts.python").ipython
    },
    -- How the repl window will be displayed
    -- See below for more information
    repl_open_cmd = require('iron.view').right("35%"),
  },
  -- Iron doesn't set keymaps by default anymore.
  -- You can set them here or manually add keymaps to the functions in iron.core
  keymaps = {
    send_motion = "<leader>rm",
    visual_send = "<leader>rv",
    send_file = "<leader>rf",
    send_line = "<leader>rl",
    send_mark = "<leader>rms",
    mark_motion = "<leader>rmm",
    mark_visual = "<leader>rmv",
    remove_mark = "<leader>rmm",
    cr = "<leader>r<cr>",
    interrupt = "<leader>r<space>",
    exit = "<leader>rq",
    clear = "<leader>rc",
  },
  -- If the highlight is on, you can change how it looks
  -- For the available options, check nvim_set_hl
  highlight = {
    italic = true
  },
  ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
}

-- Iron also has a list of commands, see :h iron-commands for all available commands
vim.keymap.set('n', '<leader>rs', '<cmd>IronRepl<cr>')
vim.keymap.set('n', '<leader>rr', '<cmd>IronRestart<cr>')
vim.keymap.set('n', '<leader>rh', '<cmd>IronHide<cr>')

-- Define the function to exit terminal mode
local function exit_terminal_mode()
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<C-\\><C-n>', true, false, true), 'n', true)
end

-- Map the Escape key to the exit_terminal_mode function in terminal mode
vim.keymap.set('t', '<Esc>', function () exit_terminal_mode() end, {noremap = true})

wk.register({
    r = {
        name = "+iron",
        m = "Send Motion",
        v = "Send Visual",
        f = "Send File",
        l = "Send Line",
        ["<cr>"] = "Send Enter",
        ["<leader>"] = "Send Interrupt",
        q = "Exit",
        c = "Clear REPL",
        m = {
            name = "+mark",
            s = "Send Mark",
            m = "Mark Motion",
            v = "Mark Visual",
            r = "Remove Mark"
        }
    },
}, { prefix = "<leader>" })

wk.register({
    r = {
        name = "+iron",
        v = "Send Visual",
        m = {
            name = "+mark",
            v = "Mark Visual",
        }
    },
}, { prefix = "<leader>", mode = "v"})
