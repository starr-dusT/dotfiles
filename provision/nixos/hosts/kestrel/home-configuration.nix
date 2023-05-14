{ config, pkgs, user, ... }:
{
  home.username = "${user}";
  home.homeDirectory = "/home/${user}";
  programs.home-manager.enable = true;


  home.packages = with pkgs; [
    firefox
    rofi
    alacritty
    gamemode
    lutris
    prismlauncher
    nitrogen
    keepassxc
    pcmanfm
    discord
    inkscape
    gruvbox-dark-gtk
    gruvbox-dark-icons-gtk
    libreoffice-fresh
    chezmoi
    rbw 
    vscodium.fhs
  ];

  gtk = {
    enable = true;
    theme = {
      name = "gruvbox-dark";
    };
  };

  home.stateVersion = "22.11";
}
