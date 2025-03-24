{ config, pkgs, user, lib, hostname, ... }:
{
  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.hostName = "${hostname}"; 
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
    uhd
    gnuradio
  ];

  # Modules 
  modules = {
    base-plus.enable = true;
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    devel = {
      engineering.enable = true;
      programming.enable = true;
    };
    programs = {
      appgate-sdp.enable = true;
      virt-manager.enable = true;
    };
    services = {
      samba-client.enable = true;
      ssh.enable = true;
    };
  };
}
