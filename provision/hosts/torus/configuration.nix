{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    ../../modules 
    ./wireguard-server.nix
    ./samba-server.nix
    ./syncthing.nix
    ./rss.nix
    ./home-assistant
    ./gitea.nix
    ./nextcloud.nix
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
  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1w" ];
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
  # Needed for wireguard-server
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
  };
  networking.firewall.enable = true;
  networking.firewall.checkReversePath = "loose";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ 80 443 ];

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
    setLdLibraryPath = true;
  };

  # Define user account.
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.bash;
  };

  environment.systemPackages = with pkgs; [
    docker-compose # Tool for defining and running multi-container Docker applications.
    python3 # Interpreted, high-level programming language known for its simplicity and versatility.
    zk # Command-line tool for interacting with Apache ZooKeeper, a centralized service for distributed systems.
    gollum # Wiki software that provides a simple, Git-based wiki engine.
  ];

  security.acme = {
    acceptTerms = true;
    defaults.email = "starrtyler88@gmail.com";
  };


  security.pam.services.nginx.setEnvironment = false;
  systemd.services.nginx.serviceConfig = {
    SupplementaryGroups = [ "shadow" ];
  };

  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    additionalModules = [ pkgs.nginxModules.pam ];
    virtualHosts = let
    SSL = { 
      enableACME = true; 
      forceSSL = true;
    }; in {
      "rss.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:8087/"; 
      });
      "media.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:8096/"; 
      });
      "git.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:3001/"; 
      });
      "workspace.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:5000/"; 
      });
      "lc.tstarr.us" = (SSL // {
        locations."/" = {
          proxyPass = "http://localhost:8065/"; 
          proxyWebsockets = true;
        };
      });
      "code.tstarr.us" = (SSL // {
        locations."/" = {
          proxyPass = "http://localhost:8443/"; 
          proxyWebsockets = true;
        };
      });
      "plot.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:8988/"; 
      });
    };
  };

  # Enable modules
  modules = {
    devel = {
      tooling.enable = true;
    };
    services = {
      jellyfin.enable = true;
    };
    system = {
      terminal.enable = true;
      ssh.enable = true;
      secrets.enable = true;
    };
  };
  # Did you read the comment?
  system.stateVersion = "23.11";
}
