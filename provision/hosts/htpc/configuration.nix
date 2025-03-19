{ config, pkgs, user, lib, hostname, ... }:
{
  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.hostName = "${hostname}"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Modules 
  modules = {
    base-plus.enable = true;
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    services = {
      samba-client.enable = true;
      ssh.enable = true;
    };
  };
}
