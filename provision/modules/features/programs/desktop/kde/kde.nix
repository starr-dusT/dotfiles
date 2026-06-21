{ inputs, ... }:
{
  flake.modules.nixos.kde =
    { config, pkgs, ... }:
    let
      user = "${config.preferences.user}";
      hostname = "${config.preferences.hostname}";
    in
    {
      imports = with inputs.self.modules.nixos; [
        desktop
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
          kscreenlocker = {
            appearance.wallpaper = "/home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
          };
          workspace = {
            lookAndFeel = "org.kde.breezedark.desktop";
            wallpaper = "/home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
          };
          session = {
            sessionRestore.restoreOpenApplicationsOnLogin = "startWithEmptySession";
          };
          panels = [
            {
              screen = 0;
              location = "top";
              floating = true;
              height = 44;
              widgets = [
                { kickoff = { }; }
                {
                  iconTasks = {
                    launchers = [
                      "applications:chromium-browser.desktop"
                      "applications:com.mitchellh.ghostty.desktop"
                      "applications:org.kde.dolphin.desktop"
                      "applications:discord.desktop"
                    ];
                  };
                }
                "org.kde.plasma.panelspacer"
                { digitalClock = { }; }
                "org.kde.plasma.panelspacer"
                {
                  systemTray.items = {
                    shown = [
                      "org.kde.plasma.volume"
                      "org.kde.plasma.networkmanagement"
                    ];
                  };
                }
                "org.kde.plasma.showdesktop"
              ];
            }
          ];
        };
      };
    };
}
