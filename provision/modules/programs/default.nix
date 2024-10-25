{ ... }:
{
  imports = [
    ./beancount.nix
    ./chrome
    ./firefox
    ./git.nix 
    ./chezmoi.nix 
    ./kitty.nix
    ./nvim.nix
    ./syncthing.nix
    ./yt-dlp.nix
  ];
}
