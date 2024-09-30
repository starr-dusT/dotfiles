{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    ./syncthing.nix
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
      browser = {
        chrome.enable = true;
        firefox.enable = true;
      };
      gnome = {
        enable = true;
        wallpaper = "file://${../../../resources/img/wallpapers/gruvbox/kestrel.png}";
      };
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
      wireguard-client = {
        enable = true;
        privateKeyFile = "/run/agenix/wireguard/kestrel";
        address = [ "192.168.3.3/24" ];
        publicKey = "bd7bbZOngl/FTdBlnbIhgCLNf6yx5X8WjiRB7E1NEQQ=";
        endpoint = "66.218.43.87";
      };
    };
  };
}
