vim.g.vimwiki_list = {
  {
    path = '~/documents/vimwiki/main',
    syntax = 'markdown',
    ext = '.md'
  }
}

vim.g.vimwiki_ext2syntax = {
  ['.md'] = 'markdown',
  ['.markdown'] = 'markdown',
  ['.mdown'] = 'markdown',
}

vim.g.vimwiki_global_ext = 0  -- don't treat all md files as vimwiki
vim.g.vimwiki_markdown_link_ext = 1
