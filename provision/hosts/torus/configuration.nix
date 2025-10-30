{ pkgs, ... }:
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

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.open = false;

  # Modules
  modules = {
    core = {
      physical.enable = true;
    };

    optional = {
      programs = {
        _plus.enable = true;
        docker = {
          enable = true;
          storageDriver = "btrfs";
        };
      };
      scripts = {
        enable = true;
        blacklist = [ "mount-engi.sh" ];
      };
      services = {
        syncthing = {
          enable = true;
          keyPath = ../../secrets/syncthing/torus/key.pem.age;
          certPath = ../../secrets/syncthing/torus/cert.pem.age;
          devices = {
            "kestrel" = {
              id = "5WWL4FE-ARZ4FHP-J33HQCH-CZKEXLN-2RAY4KW-PDI754F-3HVPZYI-VC3ESAF";
            };
            "stormwalker" = {
              id = "OTPOWIB-MRGIDWA-SDEEHJF-OPYEK6M-3TWREYD-T4YAKI5-RXOOXLP-UHRGZAO";
            };
          };
        };
      };
    };
  };
}
