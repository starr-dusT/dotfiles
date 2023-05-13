{ config, pkgs, user, ... }:

{

  home.username = "${user}";
  home.homeDirectory = "/home/${user}";

  home.stateVersion = "22.05";

  programs.home-manager.enable = true;

  programs.vscode = {
    enable = true;
    package = pkgs.vscode.fhsWithPackages (ps: with ps; [ ]);
  };

  home.packages = with pkgs; [
    brave
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
  ];

  gtk = {
    enable = true;
    theme = {
      name = "gruvbox-dark";
    };
  };


}
