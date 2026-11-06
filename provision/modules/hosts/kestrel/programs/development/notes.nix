{ ... }:
{
  flake.modules.nixos.kestrel =
    { pkgs, config, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      systemd.services.vulcan-push = {
        description = "Automatically commit and push vulcan";
        serviceConfig = {
          Type = "oneshot";
          User = "${user}";
          WorkingDirectory = "/home/${user}/Documents/10-19_self/11_vaults/11.11_vulcan";
        };
        path = [
          pkgs.git
        ];
        script = ''
          set -euo pipefail
          git add -A
          if ! git diff --cached --quiet; then
            git commit -m "Automated commit: $(date '+%Y-%m-%d %H:%M:%S')"
          fi
          git push origin master
        '';
      };

      systemd.timers.vulcan-push = {
        description = "Run vulcan-push every four hours";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5min";
          OnUnitActiveSec = "4h";
          Persistent = true;
        };
      };
    };
}
