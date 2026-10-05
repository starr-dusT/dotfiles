{ ... }:
{
  flake.modules.nixos.desktop-scripts =
    { pkgs, lib, config, ... }:
    let
      sink-switch = pkgs.writeShellScriptBin "sink-switch" (builtins.readFile ./sink-switch.sh);
      display-switch = pkgs.writeShellScriptBin "display-switch" (builtins.readFile ./display-switch.sh);
      subwoofer-volume = pkgs.writeShellScriptBin "subwoofer-volume" (
        builtins.readFile ./subwoofer-volume.sh
      );
      isKestrel = config.preferences.hostname == "kestrel";
    in
    {
      age.secrets."kube/homedb_pass" = {
        file = ../../../../../secrets/kube/homedb_pass.age;
        owner = "${config.preferences.user}";
        group = "users";
      };

      environment.systemPackages = [
        pkgs.jq
        pkgs.postgresql
        sink-switch
        display-switch
        subwoofer-volume
      ];

      # Set default display setting to kestrel-desktop for kestrel
      systemd.user.services.display-switch = lib.mkIf isKestrel {
        description = "Run script to set display settings on gnome launch";
        wantedBy = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${display-switch}/bin/display-switch kestrel-desktop";
          RemainAfterExit = true;
        };
      };
    };
}
