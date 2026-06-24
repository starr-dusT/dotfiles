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
        kde-shortcuts
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

      system.activationScripts.userAvatar.text = ''
        install -Dm644 ${../../../../../../resources/img/avatars/${user}.png} \
          /var/lib/AccountsService/icons/${user}
      '';

      system.activationScripts.systemWallpaper.text = ''
        install -Dm644 ${../../../../../../resources/img/wallpapers/${hostname}.png} \
          /var/lib/AccountsService/wallpaper
      '';
      environment.etc."plasmalogin.conf".text = ''
        [Greeter][Wallpaper][org.kde.image][General]
        Image=file:///var/lib/AccountsService/wallpaper
      '';

      home-manager.users.${user} = {
        programs.plasma = {
          enable = true;
          kscreenlocker = {
            appearance.wallpaper = "/home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
          };
          workspace = {
            clickItemTo = "open";
            colorScheme = "BreezeDark";
            lookAndFeel = "org.kde.breezedark.desktop";
            iconTheme = "breeze-dark";
            theme = "breeze-dark";
            wallpaper = "/home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
          };
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
            };
          };
          panels = [
            {
              screen = 1;
              location = "top";
              floating = false;
              height = 40;
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
                {
                  systemTray.items = {
                    shown = [
                      "org.kde.plasma.volume"
                      "org.kde.plasma.networkmanagement"
                    ];
                  };
                }
                {
                  digitalClock = {
                    time.format = "24h";
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
