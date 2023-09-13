-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Apperance
    use ('dracula/vim')
    use('norcalli/nvim-colorizer.lua')
    require("colorizer").setup()

    -- Syntax Hilight
    use ('alker0/chezmoi.vim')
    use ('nathangrigg/vim-beancount')

    -- IDE
    use('nvim-lua/plenary.nvim')
    use("nvim-treesitter/nvim-treesitter", {
        run = ":TSUpdate"
    })
    use ('nvim-telescope/telescope.nvim')
    use ('nvim-telescope/telescope-project.nvim')

    -- Editing
    use ('ThePrimeagen/harpoon')
    use ('mbbill/undotree')
    use ('folke/which-key.nvim')
    use("mickael-menu/zk-nvim")

    -- External
    require("zk").setup()
    use('mcchrish/nnn.vim')

    -- LSP
    use ('neovim/nvim-lspconfig')
    use ('hrsh7th/cmp-nvim-lsp')
    use ('hrsh7th/cmp-buffer')
    use ('hrsh7th/cmp-path')
    use ('hrsh7th/cmp-cmdline')
    use ('hrsh7th/nvim-cmp')
    use ('L3MON4D3/LuaSnip')
    use ('saadparwaiz1/cmp_luasnip')

end)
