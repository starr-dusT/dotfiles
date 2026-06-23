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

      # set user icon
      system.activationScripts.userAvatar.text = ''
        install -Dm644 ${../../../../../../resources/img/avatars/${user}.png} \
          /var/lib/AccountsService/icons/${user}
      '';

      # set default lockscreen (when no user is logged in)
      environment.etc."plasmalogin.conf".text = ''
        [Greeter][Wallpaper][org.kde.image][General]
        Image=file:///usr/${hostname}.png
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
          shortcuts = {
            kwin = {
              "Switch to Desktop 1" = "Meta+Q";
              "Switch to Desktop 2" = "Meta+W";
              "Switch to Desktop 3" = "Meta+E";
              "Switch to Desktop 4" = "Meta+R";
              "Window to Desktop 1" = "Meta+Shift+Q";
              "Window to Desktop 2" = "Meta+Shift+W";
              "Window to Desktop 3" = "Meta+Shift+E";
              "Window to Desktop 4" = "Meta+Shift+R";
              "Window Close" = "Meta+D";
              "Window Maximize" = "Meta+T";
              "Window Fullscreen" = "Meta+F";
              "Window Above Other Windows" = "Meta+S";

              "Switch to Previous Screen" = "Meta+H";
              "Switch to Next Screen" = "Meta+L";
              "Window to Previous Screen" = "Meta+Shift+H";
              "Window to Next Screen" = "Meta+Shift+L";
              "Window Quick Tile Left" = "Meta+J";
              "Window Quick Tile Right" = "Meta+K";

              "Edit Tiles" = "none"; # Unset default Meta+T
              "Overview" = "none"; # Unset default Meta+W
            };
            ksmserver = {
              "Lock Session" = "Meta+Space";
            };
            "services/chromium-browser.desktop"._launch = "Meta+!";
            "services/com.mitchellh.ghostty.desktop"._launch = "Meta+@";
            "services/org.kde.dolphin.desktop"._launch = "Meta+#";
            "services/discord.desktop"._launch = "Meta+$";
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
                    time.format = "12h";
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
