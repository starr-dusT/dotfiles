{ config, lib, pkgs, ... }:
{
  services.samba = {
    enable = true;
    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "smbnix";
        "netbios name" = "smbnix";
        "security" = "user";
        "hosts allow" = [ "192.168.3." "192.168.1." "127.0.0.1" "localhost" ];
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "nobody";
        "map to guest" = "bad user";
      };
    };

    shares = {
      private = {
        "path" = "/engi";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "force user" = "tstarr";
        "force group" = "users";
      };
      public = {
        "path" = "/engi";
        browseable = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
      };
    };
  };

  # Curiously, `services.samba` does not automatically open
  # the needed ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 445 139 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];

  environment.systemPackages = with pkgs; [ 
    cifs-utils # Utilities for mounting and managing CIFS (Common Internet File System) shares
  ];
}
