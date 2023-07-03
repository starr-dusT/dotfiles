-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Colors
    --use ('dracula/vim')

    -- IDE
    use('nvim-lua/plenary.nvim')
    use("nvim-treesitter/nvim-treesitter", {
        run = ":TSUpdate"
    })
    use ('nvim-telescope/telescope.nvim')
    use ('nvim-telescope/telescope-project.nvim')

    use ('ThePrimeagen/harpoon')
    use ('mbbill/undotree')
    use ('kdheepak/lazygit.nvim')

    use({ "iamcco/markdown-preview.nvim", 
          run = "cd app && npm install", 
          setup = function() vim.g.mkdp_filetypes = { "markdown" } end, 
          ft = { "markdown" }, 
        })
    -- LSP
    use ('neovim/nvim-lspconfig')
    use ('hrsh7th/cmp-nvim-lsp')
    use ('hrsh7th/cmp-buffer')
    use ('hrsh7th/cmp-path')
    use ('hrsh7th/cmp-cmdline')
    use ('hrsh7th/nvim-cmp')
    use ('L3MON4D3/LuaSnip')
    use ('saadparwaiz1/cmp_luasnip')
    use ('alker0/chezmoi.vim')
    use ('nathangrigg/vim-beancount')
    use ('vimwiki/vimwiki')
    use ('folke/which-key.nvim')
    use ('hkupty/iron.nvim')
    use("mickael-menu/zk-nvim")
    require("zk").setup()

    use('norcalli/nvim-colorizer.lua')
    require("colorizer").setup()

    use('loctvl842/monokai-pro.nvim')
    require("monokai-pro").setup()
    use('nvim-tree/nvim-tree.lua')
    use('mcchrish/nnn.vim')
end)
