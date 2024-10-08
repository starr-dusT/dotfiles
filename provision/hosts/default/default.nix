{ config, pkgs, user, lib, inputs, ... }:
{
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # Add non-free packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = import ../../lib/overlays.nix;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";
  
  environment.systemPackages = with pkgs; [
    inputs.agenix.packages.x86_64-linux.default 
  ];

  # Did you read the comment?
  system.stateVersion = "23.11";

  home-manager.users.${user} = {
    home.username = "${user}";
    home.homeDirectory = "/home/${user}";
    programs.home-manager.enable = true;

    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    # Did you read the comment?
    home.stateVersion = "23.11";
  };
}
