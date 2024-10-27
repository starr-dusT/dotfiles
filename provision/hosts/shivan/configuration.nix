{ config, pkgs, user, lib, ... }:
{
  imports = [ 
  ];

  # Use performance governor for sweet gaming performance!
  powerManagement.cpuFreqGovernor = "performance";

  # Set networking options
  networking.hostName = "shivan"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Enable docker 
  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  
  environment.systemPackages = with pkgs; [
  ]; 

  # Modules
  modules = {
    desktop = {
      enable = true;
      gnome.enable = true;
      peripherals.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
    };
    programs = {
      kitty.enable = true;
      chezmoi.apply = true;
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
