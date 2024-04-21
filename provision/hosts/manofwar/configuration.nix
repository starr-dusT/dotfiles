{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    #./klipper.nix
    #./moonraker.nix
    #./fluidd.nix
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
  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" "openssl-1.1.1w" ];
  nixpkgs.overlays = import ../../lib/overlays.nix;

  # Use zen kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Hardware options
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluez;
  hardware.sensor.iio.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # Set networking options
  networking.hostName = "manofwar"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  # Add fonts
  fonts.packages = with pkgs; [
    nerdfonts
  ];

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "dialout" "wheel" ]; # Enable ‘sudo’ for the user.
  };
  
  environment.systemPackages = with pkgs; [
    libraspberrypi # Library providing access to Raspberry Pi hardware features and interfaces.
  ];

  # Enable modules
  modules = {
    desktop = {
      sway.enable = false;
      browser.enable = false;
    };
    devel = {
      engineering.enable = false;
      notes.enable = false;
      python.enable = false;
      tooling.enable = false;
    };
    gaming = {
      steam.enable = false;
      emulation.enable = false;
      misc.enable = false;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = false;
      samba-client.enable = false;
      virt-manager.enable = false;
    };
    system = {
      secrets.enable = true;
      ssh.enable = true;
      terminal.enable = true;
      wireguard-client = {
        enable = false;
        #privateKeyFile = "/run/secrets/wireguard/kestrel";
        #address = [ "192.168.3.3/24" ];
        #publicKey = "bd7bbZOngl/FTdBlnbIhgCLNf6yx5X8WjiRB7E1NEQQ=";
        #endpoint = "66.218.43.87";
      };
    };
  };

  # Did you read the comment?
  system.stateVersion = "23.11";
}
