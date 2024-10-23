{ config, lib, pkgs, user, ... }:
{
 imports = [
    ./steam-deck.nix
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
      browser.enable = true;
      gnome.enable = true;
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
      syncthing = {
        enable = true;
        keyPath = ../../secrets/syncthing/bulwark/key.pem.age;
        certPath = ../../secrets/syncthing/bulwark/cert.pem.age;
        devices = {
          "kestrel" = { id = "5WWL4FE-ARZ4FHP-J33HQCH-CZKEXLN-2RAY4KW-PDI754F-3HVPZYI-VC3ESAF"; };
        };
      };
    };
    system = {
      ssh.enable = true;
      terminal.enable = true;
    };
  };
}
