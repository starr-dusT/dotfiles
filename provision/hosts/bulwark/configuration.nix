{ config, lib, pkgs, user, ... }:
{
 imports = [
    ./steam-deck.nix
    ./syncthing.nix
  ];

  # Set networking options
  networking.hostName = "bulwark"; 
  networking.firewall.checkReversePath = "loose";
  networking.firewall.enable = false;

  # Password-less root
  security.sudo.extraRules = [{ 
    users = [ "${user}" ];
    commands = [{ 
      command = "ALL" ;
      options= [ "NOPASSWD" ];
    }];
  }];

  environment.systemPackages = with pkgs; [
  ];

  # Secrets
  age.secrets."wireguard/bulwark".file = ../../secrets/wireguard/bulwark.age;

  # Modules
  modules = {
    desktop = {
      gnome = {
        enable = true;
        wallpaper = "file://${../../../resources/img/wallpapers/gruvbox/bulwark.png}";
      };
      browser.enable = true;
    };
    devel = {
      engineering.enable = false;
      notes.enable = true;
      python.enable = false;
      tooling.enable = false;
    };
    gaming = {
      steam.enable = true;
      emulation.enable = true;
      misc.enable = true;
    };
    services = {
      jellyfin.enable = false;
      peripherals.enable = false;
      samba-client.enable = true;
      virt-manager.enable = false;
    };
    system = {
      nipr.enable = true;
      ssh.enable = true;
      terminal.enable = true;
      secrets.enable = true;
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
