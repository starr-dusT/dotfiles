{ config, pkgs, user, lib, hostname, ... }:
{
  imports = [ 
    ./cloudflared.nix
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
  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true; # Needed for wireguard-server
  };

  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.open = false;

  # Modules
  modules = {
    base-plus.enable = true;
    physical.enable = true;
    programs = {
      docker ={ 
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
