# Samba for file sharing!

{ config, lib, pkgs, ... }:

let cfg = config.modules.services.samba;
in {
  options.modules.services.samba.enable = lib.mkEnableOption "samba";
  config = lib.mkIf cfg.enable {
    services.samba = {
      enable = true;
      extraConfig = ''
        browseable = yes
        smb encrypt = required
      '';
      shares = {
        homes = {
          browseable = "no";  # note: each home will be browseable; the "homes" share will not.
          "read only" = "no";
          "guest ok" = "no";
        };
      };
    };

    # Curiously, `services.samba` does not automatically open
    # the needed ports in the firewall.
    networking.firewall.allowedTCPPorts = [ 445 139 ];
    networking.firewall.allowedUDPPorts = [ 137 138 ];

    # To make SMB mounting easier on the command line
    environment.systemPackages = with pkgs; [ cifs-utils ];
  };
}
