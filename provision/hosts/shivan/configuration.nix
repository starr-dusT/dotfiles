{ config, pkgs, user, lib, hostname, ... }:
{
  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;
  
  # Modules
  modules = {
    core = {
      physical.enable = true;
      plus.enable = true;
    };
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      programming.enable = true;
    };
    programs = {
      virt-manager.enable = false;
    };
    scripts.enable = true;
    services = {
      samba-client.enable = true;
    };
  };
}
