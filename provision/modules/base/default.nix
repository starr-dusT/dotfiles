{ ... }:
{
  imports = [
    ./terminal.nix
    ./plus
    ../programs/chezmoi.nix
    ../programs/git.nix
    ../programs/nvim.nix
  ];
}
