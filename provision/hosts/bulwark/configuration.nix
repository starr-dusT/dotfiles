{ config, lib, pkgs, user, ... }:
{
 imports = [
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
  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" "openssl-1.1.1w" ];
  nixpkgs.overlays = import ../../lib/overlays.nix;

  # Hardware options
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluez;
  hardware.sensor.iio.enable = true;
  hardware.graphics.enable = true;

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
  fonts.packages= with pkgs; [
    nerdfonts
  ];

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "dialout" "wheel" ]; # Enable ‘sudo’ for the user.
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
  ];

  # Enable modules
  modules = {
    desktop = {
      sway.enable = false;
      gnome = {
        enable = true;
        wallpaper = "file://${../../../resources/img/wallpapers/gruvbox/bulwark.png}";
      };
      browser.enable = true;
    };
    devel = {
      engineering.enable = false;
      notes.enable = true;
      python.enable = false;
      tooling.enable = false;
    };
    gaming = {
      steam.enable = true;
      emulation.enable = true;
      misc.enable = true;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = false;
      samba-client.enable = true;
      virt-manager.enable = false;
    };
    system = {
      nipr.enable = true;
      ssh.enable = true;
      terminal.enable = true;
      secrets.enable = true;
      wireguard-client.enable = false;
    };
  };

  # Did you read the comment?
  system.stateVersion = "23.11"; 
}
