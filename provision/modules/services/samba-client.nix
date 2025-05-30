{ config, lib, pkgs, ... }:

let cfg = config.modules.services.samba-client;
in {
  options.modules.services.samba-client.enable = lib.mkEnableOption "samba-client";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      cifs-utils # Utilities for mounting and managing CIFS (Common Internet File System) shares
    ];
    networking.firewall.allowedTCPPorts = [ 445 139 ];
    networking.firewall.allowedUDPPorts = [ 137 138 ];
  };
}
