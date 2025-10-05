-- bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
    "ellisonleao/gruvbox.nvim",
    'norcalli/nvim-colorizer.lua',
    'alker0/chezmoi.vim',
    'nathangrigg/vim-beancount',
    'nvim-lua/plenary.nvim',
    "mickael-menu/zk-nvim",
    "nvim-treesitter/nvim-treesitter-context",
}

local opts = {}
require("lazy").setup(plugins, opts)

-- External
require("zk").setup()
require("colorizer").setup()
