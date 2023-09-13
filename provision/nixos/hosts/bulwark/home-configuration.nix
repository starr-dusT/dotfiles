{ config, pkgs, user, ... }:
{
  home.username = "${user}";
  home.homeDirectory = "/home/${user}";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    chezmoi
    rbw 
    zk
    bat
  ];

  home.stateVersion = "23.05";
}
