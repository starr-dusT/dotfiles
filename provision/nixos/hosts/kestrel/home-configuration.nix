{ config, pkgs, user, ... }:
{
  home.username = "${user}";
  home.homeDirectory = "/home/${user}";
  programs.home-manager.enable = true;


  home.packages = with pkgs; [
    firefox
    alacritty
    gamemode
    lutris
    pcmanfm
    discord
    inkscape
    libreoffice-fresh
    chezmoi
    rbw 
    vscodium.fhs
  ];

  home.stateVersion = "22.11";
}
