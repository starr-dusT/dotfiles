{ config, pkgs, user, ... }:
{
  home.username = "${user}";
  home.homeDirectory = "/home/${user}";
  programs.home-manager.enable = true;

  # Setup direnv
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  # Setup git
  programs.git = {
    enable = true;
    userName = "starr-dusT";
    userEmail = "starrtyler88@gmail.com";
  };

  home.packages = with pkgs; [
  ];

  # Enable home modules
  modules = {
    desktop = {
      kitty.enable = true;
    };
  };

  home.stateVersion = "23.11";
}
