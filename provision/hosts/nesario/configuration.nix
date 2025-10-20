{ config, pkgs, user, lib, hostname, ... }:
{
  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Modules 
  modules = {
    base-plus.enable = true;
    physical.enable = true;
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    scripts.enable = true;
    services = {
      samba-client.enable = true;
      ssh.enable = true;
    };
  };
}
