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

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Add fonts
  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  # Enable virtualisation
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";

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
  #  xserver = {
  #    enable = true;
  #    displayManager = {
  #      #defaultSession = "none+bspwm";
  #      lightdm.greeters.mini = {
  #        enable = true;
  #        #user = "tstarr";
  #        #extraConfig = ''
  #        #  [greeter]
  #        #  show-password-label = false
  #        #  invalid-password-text = Access Denied
  #        #  show-input-cursor = true
  #        #  password-alignment = left
  #        #  [greeter-theme]
  #        #  font-size = 1em
  #        #  background-image = ""
  #        #'';
  #      };
  #    };
  #  };
  #};
    xserver.enable = true;
    xserver.displayManager.sddm.enable = true;
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
  imports = [ ../../modules ];
  modules = {
    services = {
      samba-client.enable = true;
      vfio.enable = false;
    };
    devel = {
      tooling.enable = true;
      python.enable = true;
      engineering.enable = true;
    };
    gaming = {
      steam.enable = true;
    };
    desktop = {
      sway.enable = true;
    };
  };
  
  system.stateVersion = "23.05"; # Did you read the comment?
}
