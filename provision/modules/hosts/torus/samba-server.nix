{ ... }:
{
  flake.modules.nixos.torus =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        cifs-utils # Utilities for mounting and managing CIFS (Common Internet File System) shares
      ];

      # Curiously, `services.samba` does not automatically open
      # the needed ports in the firewall.
      networking.firewall.allowedTCPPorts = [
        445
        139
      ];
      networking.firewall.allowedUDPPorts = [
        137
        138
      ];
      services.samba = {
        enable = true;
        settings = {
          global = {
            "workgroup" = "WORKGROUP";
            "server string" = "smbnix";
            "netbios name" = "smbnix";
            "security" = "user";
            "hosts allow" = [
              "69.69.1."
              "69.69.2."
              "127.0.0.1"
              "localhost"
            ];
            "hosts deny" = "0.0.0.0/0";
            "guest account" = "nobody";
            "map to guest" = "bad user";
            "follow symlinks" = "yes";
            "wide links" = "yes";
            "unix extensions" = "no";
          };
        };

        settings = {
          engi = {
            "path" = "/engi";
            browseable = "yes";
            "read only" = "no";
            "guest ok" = "no";
            "force user" = "tstarr";
            "force group" = "users";
          };
          romm_library = {
            "path" = "/engi/media/roms/vault/romms";
            browseable = "yes";
            "read only" = "no";
            "guest ok" = "no";
            "force user" = "tstarr";
            "force group" = "users";
            "follow symlinks" = "yes";
            "wide links" = "yes";
          };
          romm_assets = {
            "path" = "/engi/media/roms/assets";
            browseable = "yes";
            "read only" = "no";
            "guest ok" = "no";
            "force user" = "tstarr";
            "force group" = "users";
          };
        };
      };
    };
}
