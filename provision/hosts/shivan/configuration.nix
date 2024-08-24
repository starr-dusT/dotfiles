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

  # Password-less root
  security.sudo.extraRules = [{ 
    users = [ "${user}" ];
    commands = [{ 
      command = "ALL" ;
      options= [ "NOPASSWD" ];
    }];
  }];
  
  environment.systemPackages = with pkgs; [
    distrobox # Platform for creating and managing Linux distribution images.
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
    gaming = {
      steam.enable = false;
      emulation.enable = false;
      misc.enable = false;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = true;
      samba-client.enable = true;
      virt-manager.enable = false;
    };
    system = {
      nipr = true;
      secrets.enable = true;
      ssh.enable = true;
      terminal.enable = true;
      wireguard-client.enable = false;
    };
  };

  home-manager.users.${user} = {
    modules = {
      desktop = {
        kitty.enable = true;
      };
    };
  };
}
