{ inputs, ... }:
{
  flake.modules.nixos.kde =
    { config, pkgs, ... }:
    let
      user = "${config.preferences.user}";
    in
    {
      imports = with inputs.self.modules.nixos; [
        desktop
        kde-connect
        kde-panel
        kde-shortcuts
        kde-theme
        kde-windows
      ];

      home-manager.sharedModules = [ inputs.plasma-manager.homeModules.plasma-manager ];

      services = {
        desktopManager.plasma6.enable = true;
        displayManager.plasma-login-manager.enable = true;
      };

      xdg.portal = {
        enable = true;
        extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
      };

      home-manager.users.${user} = {
        programs.plasma = {
          enable = true;
          session = {
            sessionRestore.restoreOpenApplicationsOnLogin = "startWithEmptySession";
          };
          kwin = {
            virtualDesktops = {
              rows = 1;
              names = [
                "1"
                "2"
                "3"
                "4"
              ];
            };
            titlebarButtons = {
              left = [ "more-window-actions" ];
              right = [ "close" ];
            };
          };
          powerdevil = {
            AC = {
              autoSuspend = {
                action = "nothing";
                idleTimeout = null;
              };
              turnOffDisplay = {
                idleTimeout = null;
              };
            };
            battery = {
              autoSuspend = {
                action = "nothing";
                idleTimeout = null;
              };
            };
            lowBattery = {
              autoSuspend = {
                action = "nothing";
                idleTimeout = null;
              };
            };
          };
          configFile = {
            kwinrc.Windows = {
              FocusPolicy = "FocusFollowsMouse";
              Placement = "UnderMouse";
              PerOutputVirtualDesktops = true;
            };
          };
        };
      };
    };
}
