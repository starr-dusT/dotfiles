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
    {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v1.x',
        dependencies = {
      	  -- LSP Support
      	  {'neovim/nvim-lspconfig'},
      	  {'williamboman/mason.nvim'},
      	  {'williamboman/mason-lspconfig.nvim'},
      	  -- Autocompletion
      	  {'hrsh7th/nvim-cmp'},
      	  {'hrsh7th/cmp-buffer'},
      	  {'hrsh7th/cmp-path'},
      	  {'saadparwaiz1/cmp_luasnip'},
      	  {'hrsh7th/cmp-nvim-lsp'},
      	  {'hrsh7th/cmp-nvim-lua'},
      	  -- Snippets
      	  {'L3MON4D3/LuaSnip'},
      	  {'rafamadriz/friendly-snippets'},
        }
    }
}

local opts = {}
require("lazy").setup(plugins, opts)

-- External
require("zk").setup()
require("colorizer").setup()
