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
    base-plus.enable = true;
    physical.enable = true;
    desktop = {
      enable = true;
      gnome.enable = true;
    };
    scripts.enable = true;
    services = {
      gnome-remote-desktop.enable = true;
      samba-client.enable = true;
      ssh.enable = true;
    };
  };
}
