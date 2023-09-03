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

  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set kernel modules
  boot.kernelModules = [ "sg" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set networking options
  networking.hostName = "torus"; 
  networking.networkmanager.enable = true;  
  networking.firewall.checkReversePath = "loose";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";

  # Enable virtualisation
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  virtualisation.docker.enableNvidia = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    setLdLibraryPath = true;
  };

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
    docker-compose
  ];

  # Enable user services
  #services = {
  #  syncthing = {
  #    enable = true;
  #    user = "${user}";
  #  };
  #};

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.pcscd.enable = true;
  programs.gnupg.agent = {
     enable = true;
     pinentryFlavor = "curses";
     enableSSHSupport = true;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "starrtyler88@gmail.com";
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
      virtualHosts = let
      SSL = { 
        enableACME = true; 
        addSSL = true;
      }; in {
        #"tstarr.us" = (SSL // {
        #  locations."/".proxyPass = "http://127.0.0.1:8080/"; 
        #  serverAliases = [
        #    "www.tstarr.us"
        #  ];
        #});
        "media.tstarr.us" = (SSL // {
          locations."/".proxyPass = "http://127.0.0.1:8096/"; 
        });
        "joplin.tstarr.us" = (SSL // {
          locations."/".proxyPass = "http://127.0.0.1:22300/"; 
        });
      };
  };

  # Enable modules
  imports = [ ../../modules ];
  modules = {
    services = {
      samba-server.enable = true;
      jellyfin.enable = true;
    };
    devel = {
      tooling.enable = true;
    };
  };
  
  system.stateVersion = "23.05"; # Did you read the comment?
}
