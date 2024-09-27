{ config, pkgs, user, ... }:
{
  home-manager.users.${user} = {
    home.username = "${user}";
    home.homeDirectory = "/home/${user}";
    programs.home-manager.enable = true;

    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    home.packages = with pkgs; [
    ];

    # Did you read the comment?
    home.stateVersion = "23.11";
  };
}
