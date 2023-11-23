{ config, pkgs, user, ... }:
{
  home.username = "${user}";
  home.homeDirectory = "/home/${user}";
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    chezmoi
    rbw 
  ];

  # Enable home modules
  #modules = {
  #};

  home.stateVersion = "23.11";
}
