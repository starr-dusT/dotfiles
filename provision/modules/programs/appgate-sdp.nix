{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.appgate-sdp;
in {
  options.modules.programs.appgate-sdp.enable = lib.mkEnableOption "appgate-sdp";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      appgate-sdp
      (pkgs.writeScriptBin "appgate-sdp-wrapped" ''
        #!/bin/sh
        appgate
        sudo systemctl restart resolvconf.service
      '')
    ];
    home-manager.users.${user} = {
      # Custom entry that uses wrapped binary
      xdg.desktopEntries.appgate = {
        name="Appgate SDP";
        exec="pkexec appgate-sdp-wrapped";
        icon="appgate-icon";
        terminal = false;
        type = "Application";
        mimeType= ["x-scheme-handler/appgate"];
        categories= ["Network"];
      };
    };

    boot.kernelModules = [ "tun" ];
    services.dbus.packages = [ pkgs.appgate-sdp ];
    systemd = {
      packages = [ pkgs.appgate-sdp ];
      services.appgatedriver.wantedBy =  [ "multi-user.target" ]; # https://github.com/NixOS/nixpkgs/issues/81138
    };
  };
}
