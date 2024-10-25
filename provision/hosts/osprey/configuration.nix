{ config, pkgs, user, lib, ... }:
{
  imports = [ 
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.hostName = "osprey"; 
  networking.firewall.checkReversePath = false;
  networking.firewall.enable = false;

  # Enable docker 
  virtualisation.containers.enable = true;
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
    appgate-sdp
  ];

  # Modules 
  modules = {
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    devel = {
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
    };
  };
}
