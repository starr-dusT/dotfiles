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
      browser = {
        chrome.enable = true;
        firefox.enable = true;
      };
      gnome = {
        enable = true;
        wallpaper = "file://${../../../resources/img/wallpapers/gruvbox/bulwark.png}";
      };
    };
    devel = {
      notes.enable = true;
    };
    gaming = {
      emulation.enable = true;
      steam.enable = true;
    };
    programs = {
      chezmoi.apply = true;
      kitty.enable = true;
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
