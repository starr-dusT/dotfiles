{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.services.syncthing;
in
{
  #config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      syncthingtray-minimal
    ];

    systemd.user.services.syncthing-tray = {
      description = "Syncthing Tray";
      after = [ "graphical-session.target" ];
      wants = [ "graphical-session.target" ];
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.syncthingtray-minimal}/bin/syncthingtray";
        Restart = "on-failure";
        RestartSec = 1;
        Type = "simple";
      };
    };
  #};
}
