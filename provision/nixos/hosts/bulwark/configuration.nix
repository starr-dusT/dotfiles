{ config, lib, pkgs, pkgs-unstable, user, ... }:
{
  imports = [
    ./steam-deck.nix
    ../../modules 
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
  # boot.kernelPackages = pkgs.linuxPackages_zen;

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

  # Enable zsh
  programs.zsh.enable = true;

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    git
    killall
    pciutils
    syncthing
    pinentry-curses 
    trash-cli
    unzip
    nnn
    advcpmv
    neovim
  ] ++ [
      pkgs-unstable.ungoogled-chromium
  ];

  # Enable user services
  services = {
    gvfs.enable = true; # USB automount
    blueman.enable = true;
    printing.enable = true;
    printing.drivers = [ pkgs.hplip ];
    avahi.enable = true;
    avahi.nssmdns = true;
    syncthing = {
      enable = true;
      user = "${user}";
      dataDir = "/home/${user}/sync";
      configDir = "/home/${user}/.config/syncthing";
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.pcscd.enable = true;
  programs.gnupg.agent = {
     enable = true;
     pinentryFlavor = "curses";
     enableSSHSupport = true;
  };

  # Enable modules
  modules = {
    services = {
      samba-client.enable = true;
    };
    devel = {
      tooling.enable = true;
    };
    gaming = {
      steam.enable = true;
    };
  };
  
  system.stateVersion = "23.05"; # Did you read the comment?
}
