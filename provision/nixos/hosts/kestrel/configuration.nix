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

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  services.xserver = {
    enable = true;
    layout = "us";

    desktopManager.xterm.enable = false;

    # Use nvidia drivers
    videoDrivers = [ "amdgpu" ];

    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    # Use the xmonad wm
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Add fonts
  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    neovim
    git
    haskellPackages.xmobar
    killall
    pciutils
    syncthing
    nnn
    xidlehook
    pamixer
    vifm
    play-with-mpv
    mpv
    autokey
    gnome-extension-manager
    gnome.gnome-tweaks
    pinentry-curses 
    zsh
    ripgrep
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
      user = "tstarr";
      dataDir = "/home/tstarr/sync";
      configDir = "/home/tstarr/.config/syncthing";
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


  imports = [ ../../modules ];

  modules = {
    services = {
      samba.enable = true;
      vfio.enable = true;
    };
    devel = {
      python.enable = true;
      engineering.enable = true;
    };
    gaming = {
        steam.enable = true;
    };
  };
  
  system.stateVersion = "22.11"; # Did you read the comment?
}
