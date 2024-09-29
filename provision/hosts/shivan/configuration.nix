{ config, pkgs, user, lib, ... }:
{
  imports = [ 
    ./syncthing.nix
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
      gnome = {
        enable = true;
        # TODO: Add Shivan wallpaper
        wallpaper = "file://${../../../resources/img/wallpapers/gruvbox/bulwark.png}";
      };
      browser.enable = true;
    };
    devel = {
      engineering.enable = true;
      notes.enable = true;
      python.enable = true;
      tooling.enable = true;
    };
    programs = {
      kitty.enable = true;
    };
    services = {
      peripherals.enable = true;
      samba-client.enable = true;
      virt-manager.enable = false;
    };
    system = {
      nipr = true;
      ssh.enable = true;
      terminal.enable = true;
    };
  };
}
