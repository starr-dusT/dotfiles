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

    optional = {
      desktop = {
        enable = true;
        gnome.enable = true;
      };
      scripts.enable = true;
    };
  };
}
