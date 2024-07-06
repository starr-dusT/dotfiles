{ config, pkgs, user, lib, ... }:
{
  imports = [ 
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
  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" "openssl-1.1.1w" ];
  nixpkgs.overlays = import ../../lib/overlays.nix;

  # Use zen kernel
  boot.kernelPackages = pkgs.linuxPackages_zen;

  # Hardware options
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluez;
  hardware.sensor.iio.enable = true;
  hardware.graphics.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set networking options
  networking.hostName = "kestrel"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  # Add fonts
  fonts.packages = with pkgs; [
    nerdfonts
  ];

  # Enable docker 
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";

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
  
  environment.systemPackages = with pkgs; [
      distrobox # Platform for creating and managing Linux distribution images.
  ];

  # Enable modules
  modules = {
    desktop = {
      sway.enable = false;
      gnome = {
        enable = true;
        wallpaper = "file://${../../../resources/img/wallpapers/gruvbox/kestrel.png}";
      };
      browser.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      python.enable = true;
      tooling.enable = true;
    };
    gaming = {
      steam.enable = true;
      emulation.enable = true;
      misc.enable = true;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = true;
      samba-client.enable = true;
      virt-manager.enable = true;
    };
    system = {
      nipr.enable = true;
      secrets.enable = true;
      ssh.enable = true;
      terminal.enable = true;
      wireguard-client = {
        enable = true;
        privateKeyFile = "/run/secrets/wireguard/kestrel";
        address = [ "192.168.3.3/24" ];
        publicKey = "bd7bbZOngl/FTdBlnbIhgCLNf6yx5X8WjiRB7E1NEQQ=";
        endpoint = "66.218.43.87";
      };
    };
  };

  # Did you read the comment?
  system.stateVersion = "23.11";
}
