{ ... }:
{
  flake.modules.nixos.samba-client =
    { pkgs, config, ... }:
    {
      environment.systemPackages = with pkgs; [
        cifs-utils # Utilities for mounting and managing CIFS (Common Internet File System) shares
      ];
      networking.firewall.allowedTCPPorts = [
        445
        139
      ];
      networking.firewall.allowedUDPPorts = [
        137
        138
      ];
      # Expose creds for smb shares
      age.secrets."smb/torus" = {
        file = ../../../../../secrets/smb/torus.age;
        owner = "${config.preferences.user}";
        group = "users";
      };
    };
}
