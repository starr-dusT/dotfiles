require('telescope').setup {
  extensions = {
    project = {
      base_dirs = {
        '~/.dotfiles',
        '~/devel/work/genisys/splat-react',
        '~/devel/work/genisys/splat-python',
      },
      hidden_files = true,
      sync_with_nvim_tree = true, -- default false
      no_ignore = true,
    }
  }
}
