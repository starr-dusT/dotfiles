{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    ./backup.nix
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
      browser.enable = true;
      gnome.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      python.enable = true;
      tooling.enable = true;
    };
    gaming = {
      emulation.enable = true;
      minecraft.enable = true;
      steam.enable = true;
    };
    programs = {
      chezmoi.apply = true;
      kitty.enable = true;
      syncthing = {
        enable = true;
        keyPath = ../../secrets/syncthing/kestrel/key.pem.age;
        certPath = ../../secrets/syncthing/kestrel/cert.pem.age;
        devices = {
          "bulwark" = { id = "YKPOWTQ-XMXG3SD-XKLPVEC-H4SO345-2ZZQK65-EBISRED-ISKCFMQ-T74P6Q5"; };
        };
      };
    };
    services = {
      peripherals.enable = true;
      samba-client.enable = true;
      virt-manager.enable = true;
    };
    system = {
      backup.enable = true;
      ssh.enable = true;
      terminal.enable = true;
    };
  };
}
