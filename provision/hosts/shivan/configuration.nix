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
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set networking options
  networking.hostName = "shivan"; 
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

  # Enable docker 
  virtualisation.docker.enable = true;

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
      # One-off packages
      distrobox
  ]; 

  # Enable modules
  modules = {
    desktop = {
      sway.enable = true;
      browser.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      python.enable = true;
      tooling.enable = true;
    };
    gaming = {
      steam.enable = false;
      emulation.enable = false;
      misc.enable = false;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = true;
      samba-client.enable = true;
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
