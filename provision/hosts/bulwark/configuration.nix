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
      notes.enable = true;
    };
    gaming = {
      steam.enable = true;
      emulation.enable = true;
      misc.enable = true;
    };
    programs = {
      kitty.enable = true;
    };
    services = {
      samba-client.enable = true;
    };
    system = {
      nipr.enable = true;
      ssh.enable = true;
      terminal.enable = true;
    };
  };
}
