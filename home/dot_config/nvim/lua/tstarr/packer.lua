-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Apperance
    use("ellisonleao/gruvbox.nvim")
    use('norcalli/nvim-colorizer.lua')
    require("colorizer").setup()

    -- Syntax Hilight
    use ('alker0/chezmoi.vim')
    use ('nathangrigg/vim-beancount')

    -- IDE    
    use('nvim-lua/plenary.nvim')

    -- Editing
    use ('folke/which-key.nvim')
    use("mickael-menu/zk-nvim")

    -- External
    require("zk").setup()

    -- LSP
    use("nvim-treesitter/nvim-treesitter-context");
    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v1.x',
        requires = {
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

end)
