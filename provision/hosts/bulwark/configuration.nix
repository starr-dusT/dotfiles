{ config, lib, pkgs, pkgs-unstable, user, ... }:
{
  imports = [
    ../../modules 
    ./steam-deck.nix
    ./syncthing.nix
  ];

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

  # Custom kernel is set within Jovian-Nixos
  #boot.kernelPackages = pkgs.linuxPackages_zen;

  # Hardware options
  hardware.bluetooth.enable = true;
  hardware.sensor.iio.enable = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set networking options
  networking.hostName = "bulwark"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  # Add fonts
  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "dialout" "wheel" "docker" "libvirtd" ]; # Enable ‘sudo’ for the user.
  };

  # Password-less root
  security.sudo.extraRules = [{ 
    users = [ "${user}" ];
    commands = [{ 
      command = "ALL" ;
      options= [ "NOPASSWD" ];
    }];
  }];

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
      # One-off stable packages
      firefox
      google-chrome
      discord
      gamemode
  ] ++ [
      # One-off unstable packages
  ];

  # Enable modules
  modules = {
    desktop = {
      sway.enable = false;
    };
    devel = {
      engineering.enable = false;
      notes.enable = true;
      python.enable = false;
      tooling.enable = false;
    };
    gaming = {
      steam.enable = true;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = false;
      samba-client.enable = true;
      virt-manager.enable = false;
    };
    system = {
      ssh.enable = true;
      terminal.enable = true;
    };
  };
  # Did you read the comment?
  system.stateVersion = "23.05"; 
}