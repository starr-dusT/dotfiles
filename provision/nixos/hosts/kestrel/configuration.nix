{ config, pkgs, user, lib, ... }:
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

  # Use zen kernel
  boot.kernelPackages = pkgs.linuxPackages_zen;

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
  networking.hostName = "kestrel"; 
  networking.networkmanager.enable = true;  
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  # Add fonts
  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  # Enable virtualisation
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "dialout" "wheel" "docker" "libvirtd" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
      # One-off stable packages
  ] ++ [
      # One-off unstable packages
  ];

  # Enable modules
  imports = [ ../../modules ];
  modules = {
    desktop = {
      sway.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      python.enable = true;
      tooling.enable = true;
    };
    gaming = {
      steam.enable = true;
    };
    services = {
      #jellyfin.enable = true;
      peripherals.enable = true;
      samba-client.enable = true;
      #samba-server.enable = true;
      syncthing.enable = true;
      virt-manager.enable = true;
    };
    system = {
      ssh.enable = true;
      terminal.enable = true;
    };
  };
  # Did you read the comment?
  system.stateVersion = "23.05";
}
