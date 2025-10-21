{ config, pkgs, user, lib, hostname, ... }:
{
  imports = [ 
    ../kestrel/gnome.nix
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Modules 
  modules = {
    core = {
      physical.enable = true;
    };

    optional = {
      desktop = {
        enable = true;
        gnome.enable = true;
        gnome-remote-desktop.enable = true;
      };
      programs = {
        _plus.enable = true;
      };
      scripts.enable = true;
    };
  };
}
