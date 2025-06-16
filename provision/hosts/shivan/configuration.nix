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
      peripherals.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      programming.enable = true;
    };
    programs = {
      docker = {
        enable = true;
        storageDriver = "btrfs";
      };
      virt-manager.enable = false;
    };
    services = {
      samba-client.enable = true;
    };
    system = {
      ssh.enable = true;
      terminal.enable = true;
    };
  };
}
