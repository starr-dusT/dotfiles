{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    ./backup.nix
    ./gnome.nix
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.hostName = "kestrel"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Nvidia options
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    modesetting.enable = true;
    open = true;
    powerManagement.enable = true;
    nvidiaSettings = true;
  };

  # Enable docker 
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  
  environment.systemPackages = with pkgs; [
  ];

  # Secrets
  age.secrets."wireguard/kestrel".file = ../../secrets/wireguard/kestrel.age;

  # Modules 
  modules = {
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
    };
    gaming = {
      emulation.enable = true;
      minecraft.enable = true;
      steam.enable = true;
    };
    programs = {
      beancount.enable = true;
      borg.enable = true;
      chezmoi.apply = true;
      kitty.enable = true;
      virt-manager.enable = true;
    };
    services = {
      samba-client.enable = true;
      ssh.enable = true;
      syncthing = {
        enable = true;
        keyPath = ../../secrets/syncthing/kestrel/key.pem.age;
        certPath = ../../secrets/syncthing/kestrel/cert.pem.age;
        devices = {
          "bulwark" = { id = "YKPOWTQ-XMXG3SD-XKLPVEC-H4SO345-2ZZQK65-EBISRED-ISKCFMQ-T74P6Q5"; };
          "torus" = { id = "ZVABUCA-3SA5QKR-OZSCIS5-RDAHR2V-D4R4NFK-ZBYOKDP-6HQUG2M-BNL3DAO"; };
        };
      };
    };
  };
}
