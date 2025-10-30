{ config, ... }:
{
  imports = [
    ./backup.nix
    ./gnome.nix
    ./vulcan.nix
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Nvidia options
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    modesetting.enable = true;
    open = true;
    powerManagement.enable = true;
    nvidiaSettings = true;
  };

  # Secrets
  age.secrets."wireguard/kestrel".file = ../../secrets/wireguard/kestrel.age;

  # Wireguard clients
  environment.etc."NetworkManager/system-connections/kestrel.nmconnection" = {
    mode = "0600";
    source = config.age.secrets."wireguard/kestrel".path;
  };

  # Modules
  modules = {
    core = {
      physical.enable = true;
    };

    optional = {
      desktop = {
        enable = true;
        gnome.enable = true;
        gnome-remote-desktop.enable = true;
      };
      development = {
        engineering.enable = true;
        notes.enable = true;
        programming.enable = true;
      };
      gaming = {
        emulation.enable = true;
        minecraft.enable = true;
        pc.enable = true;
      };
      programs = {
        _plus.enable = true;
        docker = {
          enable = true;
          storageDriver = "btrfs";
        };
        virt-manager.enable = true;
      };
      scripts.enable = true;
      services = {
        syncthing = {
          enable = true;
          keyPath = ../../secrets/syncthing/kestrel/key.pem.age;
          certPath = ../../secrets/syncthing/kestrel/cert.pem.age;
          devices = {
            "kruos" = {                                                                                                                  
              id = "VROOKEG-H75SHUK-FK2LRAI-DU77ULS-WB6ZMQ2-7AIE5SQ-IDZLU7Q-Z762ZQU";                                                    
            };                                                                                                                           
            "stormwalker" = {
              id = "OTPOWIB-MRGIDWA-SDEEHJF-OPYEK6M-3TWREYD-T4YAKI5-RXOOXLP-UHRGZAO";
            };
            "torus" = {
              id = "ZVABUCA-3SA5QKR-OZSCIS5-RDAHR2V-D4R4NFK-ZBYOKDP-6HQUG2M-BNL3DAO";
            };
          };
        };
      };
    };
  };
}
