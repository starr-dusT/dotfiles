{ config, lib, inputs, pkgs, user, home-manager, hostname, ... }:
{
  imports = [
    ./terminal.nix
    ./physical.nix
    ./plus.nix
  ];

  # base nix options
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = "experimental-features = nix-command flakes";
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # Set networking options
  networking.hostName = "${hostname}"; 

  # Add user age key to identity path
  age.identityPaths = [ 
    "/home/${user}/.ssh/keys/age"
    "/etc/ssh/ssh_host_ed25519_key"
    "/etc/ssh/ssh_host_rsa_key"
  ];

  # Add non-free packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = import ../../lib/overlays.nix;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";
  
  # system packages
  environment.systemPackages = with pkgs; [
    inputs.agenix.packages.x86_64-linux.default 
  ];

  # define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "dialout" "wheel" "docker" "libvirtd" ];
    shell = pkgs.bash;
  };

  # Did you read the comment?
  system.stateVersion = "23.11";

  # base home manager config
  home-manager.users.${user} = {
    home.username = "${user}";
    home.homeDirectory = "/home/${user}";
    programs.home-manager.enable = true;

    # Did you read the comment?
    home.stateVersion = "23.11";
  };
}
