-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Colors
  use 'navarasu/onedark.nvim'

  -- IDE
  use('nvim-lua/plenary.nvim')
  use("nvim-treesitter/nvim-treesitter", {
      run = ":TSUpdate"
  })
  use ('nvim-telescope/telescope.nvim')
  use ('nvim-telescope/telescope-project.nvim')

  use ('ThePrimeagen/harpoon')
  use ('kdheepak/lazygit.nvim')
  use 'neovim/nvim-lspconfig'

end)
