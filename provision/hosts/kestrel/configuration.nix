{ config, pkgs, user, lib, hostname, ... }:
{
  imports = [ 
    ./backup.nix
    ./gnome.nix
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Nvidia options
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    modesetting.enable = true;
    open = true;
    powerManagement.enable = true;
    nvidiaSettings = true;
  };
  
  # Secrets
  age.secrets."wireguard/kestrel".file = ../../secrets/wireguard/kestrel.age;

  # Modules 
  modules = {
    base-plus.enable = true;
    physical.enable = true;
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    devel = {
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
      docker ={ 
        enable = true;
        storageDriver = "btrfs";
      };
      virt-manager.enable = true;
    };
    services = {
      gnome-remote-desktop.enable = true;
      samba-client.enable = true;
      ssh.enable = true;
      syncthing = {
        enable = true;
        keyPath = ../../secrets/syncthing/kestrel/key.pem.age;
        certPath = ../../secrets/syncthing/kestrel/cert.pem.age;
        devices = {
          "torus" = { id = "ZVABUCA-3SA5QKR-OZSCIS5-RDAHR2V-D4R4NFK-ZBYOKDP-6HQUG2M-BNL3DAO"; };
        };
      };
    };
  };
}
