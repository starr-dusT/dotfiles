{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    ./wireguard-server.nix
    ./samba-server.nix
    ./rss.nix
    ./home-assistant
    ./gitea.nix
    ./nextcloud.nix
    ./backup.nix
    ./jellyfin.nix
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
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true; # Needed for wireguard-server
  };

  # Enable virtualisation
  virtualisation.docker = {
    enable = true;
    package = pkgs.docker_27;
    storageDriver = "btrfs";
  };

  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.open = false;

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
        extraConfig = ''
          client_max_body_size 3000m;
        '';
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
    programs = {
      chezmoi.apply = true;
      borg.enable = true;
    };
    services = {
      ssh.enable = true;
      syncthing = {
        enable = true;
        keyPath = ../../secrets/syncthing/torus/key.pem.age;
        certPath = ../../secrets/syncthing/torus/cert.pem.age;
        devices = {
          "bulwark" = { id = "YKPOWTQ-XMXG3SD-XKLPVEC-H4SO345-2ZZQK65-EBISRED-ISKCFMQ-T74P6Q5"; };
          "kestrel" = { id = "5WWL4FE-ARZ4FHP-J33HQCH-CZKEXLN-2RAY4KW-PDI754F-3HVPZYI-VC3ESAF"; };
        };
      };
    };
  };
}
