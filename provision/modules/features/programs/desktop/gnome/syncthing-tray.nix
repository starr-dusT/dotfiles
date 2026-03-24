{ ... }:
{
  flake.modules.nixos.syncthing-tray =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        syncthingtray # Tray icon and further platform integrations for Syncthing
      ];

      systemd.user.services.syncthing-tray = {
        description = "Syncthing Tray";
        after = [ "graphical-session.target" ];
        wants = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = "${pkgs.syncthingtray}/bin/syncthingtray";
          Restart = "on-failure";
          RestartSec = 1;
          Type = "simple";
        };
      };
    };
}
