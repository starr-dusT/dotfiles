{ config, pkgs, user, lib, hostname, ... }:
{
  imports = [ 
    ./wireguard-server.nix
    ./samba-server.nix
    ./rss.nix
    ./home-assistant
    ./gitea.nix
    ./backup.nix
    ./jellyfin.nix
  ];

  # Use normal kernel
  boot.kernelPackages = pkgs.linuxPackages;

  # Set kernel modules
  boot.kernelModules = [ "sg" ];

  # Set networking options
  networking.firewall.enable = true;
  networking.firewall.checkReversePath = "loose";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ 80 443 ];
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true; # Needed for wireguard-server
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
    clientMaxBodySize = "200M";
    additionalModules = [ pkgs.nginxModules.pam ];
    virtualHosts = let
    SSL = { 
      enableACME = true; 
      forceSSL = true;
    }; in {
      "rss.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:8087/"; 
      });
      "link.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:9090/"; 
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
      "cloud.tstarr.us" = (SSL // {
        locations."/".proxyPass = "http://localhost:8080/"; 
        extraConfig = ''
          client_max_body_size 3000m;
        '';
      });
      "lc.tstarr.us" = (SSL // {
        locations."/" = {
          proxyPass = "http://localhost:8065/"; 
          proxyWebsockets = true;
        }; 
      });
      "codeA.tstarr.us" = (SSL // {
        locations."/" = {
          proxyPass = "http://localhost:3000/"; 
          proxyWebsockets = true;
        };
      });
      "codeB.tstarr.us" = (SSL // {
        locations."/" = {
          proxyPass = "http://localhost:3002/"; 
          proxyWebsockets = true;
        };
      });
    };
  };

  # Modules
  modules = {
    base-plus.enable = true;
    physical.enable = true;
    programs = {
      docker = {
        enable = true;
        storageDriver = "btrfs";
      };
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
