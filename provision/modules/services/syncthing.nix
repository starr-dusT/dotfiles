{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.syncthing;
in {
  options.modules.services.syncthing = with lib; {
    enable = lib.mkOption {
      type = types.bool;
      default = true;
    };
    keyPath = mkOption {
      type = types.path;
      default = ./key.pem;
    };
    certPath = mkOption {
      type = types.path;
      default = ./cert.pem;
    };
    devices = mkOption {
      type = types.attrs;
      default = {}; 
      description = ''
        A set of devices and associated IDs.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      syncthing # File sync program for multiple devices in real-time.
    ];

    networking.firewall.allowedTCPPorts = [ 8384 22000 ];
    networking.firewall.allowedUDPPorts = [ 22000 21027 ];

    age.secrets."syncthing/key.pem" = {
      file = cfg.keyPath;
      owner = "${user}";
      group = "users";
    };
    age.secrets."syncthing/cert.pem" = {
      file = cfg.certPath;
      owner = "${user}";
      group = "users";
    };

    services.syncthing = {
      enable = true;
      user = "${user}";
      dataDir = "/home/${user}/.local/share/syncthing";
      configDir = "/home/${user}/.config/syncthing";
      guiAddress = "0.0.0.0:8384";
      key = "/run/agenix/syncthing/key.pem";
      cert = "/run/agenix/syncthing/cert.pem";
      settings.devices = cfg.devices;
    };
  };
}
