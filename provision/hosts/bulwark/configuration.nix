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
      enable = true;
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
      ssh.enable = true;
      syncthing = {
        enable = true;
        keyPath = ../../secrets/syncthing/bulwark/key.pem.age;
        certPath = ../../secrets/syncthing/bulwark/cert.pem.age;
        devices = {
          "kestrel" = { id = "5WWL4FE-ARZ4FHP-J33HQCH-CZKEXLN-2RAY4KW-PDI754F-3HVPZYI-VC3ESAF"; };
          "torus" = { id = "ZVABUCA-3SA5QKR-OZSCIS5-RDAHR2V-D4R4NFK-ZBYOKDP-6HQUG2M-BNL3DAO"; };
        };
      };
    };
  };
}
