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

  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set kernel modules
  boot.kernelModules = [ "sg" ];

  # Set networking options
  networking.hostName = "torus"; 
  networking.firewall.enable = true;
  networking.firewall.checkReversePath = "loose";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ 80 443 ];
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true; # Needed for wireguard-server
  };

  # Enable virtualisation
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  virtualisation.docker.enableNvidia = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  environment.systemPackages = with pkgs; [
    docker-compose # Tool for defining and running multi-container Docker applications.
    python3 # Interpreted, high-level programming language known for its simplicity and versatility.
  ];

  security.acme = {
    acceptTerms = true;
    defaults.email = "starrtyler88@gmail.com";
  };

  # Nginx
  security.pam.services.nginx.setEnvironment = false;
  systemd.services.nginx.serviceConfig = { SupplementaryGroups = [ "shadow" ]; };
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

  # Modules
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

  home-manager.users.${user} = {
    modules = {
      desktop = {
        kitty.enable = false;
      };
    };
  };
}
