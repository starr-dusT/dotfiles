{ config, pkgs, user, ... }:
{
  home.username = "${user}";
  home.homeDirectory = "/home/${user}";
  programs.home-manager.enable = true;

  # Setup git
  programs.git = {
      enable = true;
      userName = "starr-dusT";
      userEmail = "starrtyler88@gmail.com";
  };

  home.packages = with pkgs; [
  ];

  home.stateVersion = "23.05";
}
