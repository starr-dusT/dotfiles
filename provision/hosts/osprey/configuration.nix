{ config, pkgs, user, lib, ... }:
{
  imports = [ 
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.hostName = "osprey"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Enable docker 
  virtualisation.containers.enable = true;
  virtualisation.docker.enable = true;
  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };
  
  environment.systemPackages = with pkgs; [
    docker-compose
    podman-tui
    dive
  ];

  # Modules 
  modules = {
    desktop = {
      browser.enable = true;
      gnome.enable = true;
    };
    devel = {
      python.enable = true;
      tooling.enable = true;
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
      ssh.enable = true;
      terminal.enable = true;
      wireguard-client = {
        enable = true;
        privateKeyFile = "/run/agenix/wireguard/osprey";
        address = [ "192.168.3.3/24" ];
        publicKey = "bd7bbZOngl/FTdBlnbIhgCLNf6yx5X8WjiRB7E1NEQQ=";
        endpoint = "66.218.43.87";
      };
    };
  };
}
