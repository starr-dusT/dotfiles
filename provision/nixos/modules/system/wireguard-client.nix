{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.wireguard-client;

in {
  options.modules.system.wireguard-client = with lib; {
    enable = lib.mkEnableOption "wireguard-client";
    privateKeyFile = lib.mkOption { type = with types; str; };
    address = lib.mkOption { type = with types; listOf str; };
    publicKey = lib.mkOption { type = with types; str; };
    endpoint = lib.mkOption { type = with types; str; };
    autostart = lib.mkOption {
      type = with types; bool;
      default = false;
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall = {
      allowedUDPPorts = [ 51820 ];
    };
    networking.wg-quick.interfaces = {
      wg0 = {
        address = cfg.address;
        listenPort = 51820; 
        privateKeyFile = cfg.privateKeyFile;
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
