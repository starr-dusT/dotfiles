{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.wireguard-client;

in {
  options.modules.system.wireguard-client = with lib; {
    enable = lib.mkEnableOption "wireguard-client";
    host = lib.mkOption { type = with types; str; };
    address = lib.mkOption { type = with types; listOf str; };
    publicKey = lib.mkOption { type = with types; str; };
    endpoint = lib.mkOption { type = with types; str; };
    autostart = lib.mkOption {
      type = with types; bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    # Create qr code for phones with:
    # qrencode -t ansiutf8 < myfile_here
    environment.systemPackages = with pkgs; [ 
      qrencode # Command-line utility for generating QR codes from text or data.
    ];
    networking.firewall = {
      allowedUDPPorts = [ 51820 ];
    };
    # deploy needed secrets
    age.secrets."wireguard/${cfg.host}".file = ../../secrets/wireguard/${cfg.host}.age;
    networking.wg-quick.interfaces = {
      wg0 = {
        address = cfg.address;
        listenPort = 51820; 
        privateKeyFile = "/run/agenix/wireguard/${cfg.host}";
        autostart = cfg.autostart;
        peers = [{
            publicKey = cfg.publicKey;
            allowedIPs = [ "0.0.0.0/0" "::/0" ];
            endpoint = "${cfg.endpoint}:51820"; 
            persistentKeepalive = 25;
        }];
      };
    };
  };
}
