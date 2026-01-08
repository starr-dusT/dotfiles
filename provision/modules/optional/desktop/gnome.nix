{
  config,
  lib,
  pkgs,
  user,
  hostname,
  ...
}:

let
  cfg = config.modules.optional.desktop.gnome;
  inherit (builtins) attrNames map;
  inherit (lib.attrsets) mapAttrs' nameValuePair;
  generate_custom_keybindings =
    binds:
    {
      "org/gnome/settings-daemon/plugins/media-keys" = {
        custom-keybindings = map (
          name: "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/${name}/"
        ) (attrNames binds);
      };
    }
    // mapAttrs' (
      name: nameValuePair "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/${name}"
    ) binds;
in
{
  options.modules.optional.desktop.gnome.enable = lib.mkEnableOption "gnome";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dconf-editor # Graphical tool for editing settings stored in the dconf database of GNOME
      evolution # Personal information management application that provides email, calendar, and contact management features
      gnomeExtensions.custom-hot-corners-extended # GNOME Shell extension for changing window focus behavior
      gnomeExtensions.alphabetical-app-grid # Alphabetically order the app grid and folders
      gnome-set-panel-monitor # Set monitor for panel to appear on
      ftw # Build custom ftl wallpapers in a complicated way for no reason
    ];

    environment.gnome.excludePackages = with pkgs; [
      baobab # Disk usage analyzer for the GNOME desktop environment
      cheese # Webcam application for taking photos and videos
      epiphany # Web browser for the GNOME desktop environment
      gedit # Text editor for the GNOME desktop environment
      simple-scan # Simple scanning utility for scanning documents and images
      totem # Movie player for the GNOME desktop environment
      yelp # Help viewer application for the GNOME desktop environment
      evince # Document viewer for the GNOME desktop environment
      geary # Email client for the GNOME desktop environment
      seahorse # GNOME application for managing encryption keys and passwords
      gnome-tour # Guided tour application for introducing users to GNOME desktop environment features
      snapshot # Utility for taking and managing system snapshots in the GNOME desktop environment
      gnome-connections # GNOME application for accessing remote machines and services
      gnome-font-viewer # Utility for previewing and managing fonts in the GNOME desktop environment
      gnome-logs # Log viewer application for GNOME
      gnome-maps # Map application for the GNOME desktop environment
      gnome-music # Music player and management application for GNOME
      gnome-shell-extensions # Extensions for enhancing functionality and customization in the GNOME
    ];

    services = {
      gvfs.enable = true;
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
      desktopManager.gnome.enable = true;
    };

    xdg.mime = {
      enable = true;
      defaultApplications = {
        "text/plain" = "org.gnome.TextEditor.desktop";
        "image/bmp" = "org.gnome.Loupe.desktop";
        "image/gif" = "org.gnome.Loupe.desktop";
        "image/jpg" = "org.gnome.Loupe.desktop";
        "image/pjpeg" = "org.gnome.Loupe.desktop";
        "image/png" = "org.gnome.Loupe.desktop";
        "image/tiff" = "org.gnome.Loupe.desktop";
        "image/webp" = "org.gnome.Loupe.desktop";
        "image/x-bmp" = "org.gnome.Loupe.desktop";
        "image/x-gray" = "org.gnome.Loupe.desktop";
        "image/x-icb" = "org.gnome.Loupe.desktop";
        "image/x-ico" = "org.gnome.Loupe.desktop";
        "image/x-png" = "org.gnome.Loupe.desktop";
        "image/x-portable-anymap" = "org.gnome.Loupe.desktop";
        "image/x-portable-bitmap" = "org.gnome.Loupe.desktop";
        "image/x-portable-graymap" = "org.gnome.Loupe.desktop";
        "image/x-portable-pixmap" = "org.gnome.Loupe.desktop";
        "image/x-xbitmap" = "org.gnome.Loupe.desktop";
        "image/x-xpixmap" = "org.gnome.Loupe.desktop";
        "image/x-pcx" = "org.gnome.Loupe.desktop";
        "image/svg+xml" = "org.gnome.Loupe.desktop";
        "image/svg+xml-compressed" = "org.gnome.Loupe.desktop";
        "image/vnd.wap.wbmp" = "org.gnome.Loupe.desktop";
        "image/x-icns" = "org.gnome.Loupe.desktop";
      };
    };

    home-manager.users.${user} =
      { lib, ... }:
      {
        # Remove unwanted desktop entries
        xdg.desktopEntries.cups = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.math = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.calc = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.draw = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.base = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.xterm = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.writer = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };
        xdg.desktopEntries.impress = {
          name = "";
          exec = null;
          settings.Hidden = "true";
        };

        dconf.settings =
          let
            inherit (lib.hm.gvariant) mkTuple mkUint32 mkVariant;
            bakersfield = (
              mkVariant (mkTuple [
                (mkUint32 2)
                (mkVariant (mkTuple [
                  "Bakersfield"
                  "KBFL"
                  true
                  [
                    (mkTuple [
                      0.61843317782088048
                      (-2.0779308356004798)
                    ])
                  ]
                  [
                    (mkTuple [
                      0.61738041266937005
                      (-2.0772684133361778)
                    ])
                  ]
                ]))
              ])
            );
            tehran = (
              mkVariant (mkTuple [
                (mkUint32 2)
                (mkVariant (mkTuple [
                  "Tehran"
                  "OIII"
                  true
                  [
                    (mkTuple [
                      0.62279164893554573
                      0.8962265708990883
                    ])
                  ]
                  [
                    (mkTuple [
                      0.62316980942457534
                      0.89747622664351612
                    ])
                  ]
                ]))
              ])
            );
            moscow = (
              mkVariant (mkTuple [
                (mkUint32 2)
                (mkVariant (mkTuple [
                  "Moscow"
                  "UUWW"
                  true
                  [
                    (mkTuple [
                      0.97127572873484425
                      0.65042604039431762
                    ])
                  ]
                  [
                    (mkTuple [
                      0.97305983920281813
                      0.65651530216830811
                    ])
                  ]
                ]))
              ])
            );
            beijing = (
              mkVariant (mkTuple [
                (mkUint32 2)
                (mkVariant (mkTuple [
                  "Beijing"
                  "ZBAA"
                  true
                  [
                    (mkTuple [
                      0.69696814214530467
                      2.0295270260429752
                    ])
                  ]
                  [
                    (mkTuple [
                      0.69689057971334611
                      2.0313596217575696
                    ])
                  ]
                ]))
              ])
            );
          in
          lib.recursiveUpdate
            {
              # Settings for gnome and default applications
              "org/gnome/desktop/interface" = {
                color-scheme = "prefer-dark";
                accent-color = "purple";
                enable-animations = false;
              };
              "org/gnome/desktop/background" = {
                picture-uri = "file:///home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
                picture-uri-dark = "file:///home/${user}/.local/share/chezmoi/resources/img/wallpapers/${hostname}.png";
              };
              "org/gnome/shell" = {
                favorite-apps = [
                  "chromium-browser.desktop"
                  "com.mitchellh.ghostty.desktop"
                  "org.gnome.Nautilus.desktop"
                  "obsidian.desktop"
                  "steam.desktop"
                  "discord.desktop"
                ];
                disable-user-extensions = false;
                enabled-extensions = [
                  "gnome-shell-extension-set-panel-monitor@tstarr.us"
                  "AlphabeticalAppGrid@stuarthayhurst"
                ];
              };
              "org/gnome/shell/app-switcher" = {
                current-workspace-only = true;
              };
              "org/gnome/mutter" = {
                overlay-key = "Super";
                center-new-windows = true;
                workspaces-only-on-primary = true;
                dynamic-workspaces = false;
                experimental-features = [
                  "scale-monitor-framebuffer"
                  "xwayland-native-scaling"
                ];
              };
              "org/gnome/mutter/keybindings" = {
                switch-monitor = [ ];
              };
              "org/gnome/desktop/wm/preferences" = {
                focus-mode = "sloppy";
                num-workspaces = 10;
              };
              "org/gnome/shell/keybindings" = {
                switch-to-application-1 = [ ];
                switch-to-application-2 = [ ];
                switch-to-application-3 = [ ];
                switch-to-application-4 = [ ];
                switch-to-application-5 = [ ];
                switch-to-application-6 = [ ];
                switch-to-application-7 = [ ];
                switch-to-application-8 = [ ];
                switch-to-application-9 = [ ];
                toggle-quick-settings = [ ];
              };
              "org/gnome/settings-daemon/plugins/media-keys" = {
                play = [ "<Super>p" ];
              };
              "org/gnome/settings-daemon/plugins/power" = {
                sleep-inactive-ac-type = "blank";
                sleep-inactive-battery-type = "blank";
                sleep-inactive-ac-timeout = 600;
                sleep-inactive-battery-timeout = 600;
              };
              "org/gnome/desktop/wm/keybindings" = {
                switch-to-workspace-1 = [ "<Super>1" ];
                switch-to-workspace-2 = [ "<Super>2" ];
                switch-to-workspace-3 = [ "<Super>3" ];
                switch-to-workspace-4 = [ "<Super>4" ];
                switch-to-workspace-5 = [ "<Super>5" ];
                switch-to-workspace-6 = [ "<Super>6" ];
                switch-to-workspace-7 = [ "<Super>7" ];
                switch-to-workspace-8 = [ "<Super>8" ];
                switch-to-workspace-9 = [ "<Super>9" ];
                switch-to-workspace-0 = [ "<Super>0" ];
                move-to-workspace-1 = [ "<Shift><Super>1" ];
                move-to-workspace-2 = [ "<Shift><Super>2" ];
                move-to-workspace-3 = [ "<Shift><Super>3" ];
                move-to-workspace-4 = [ "<Shift><Super>4" ];
                move-to-workspace-5 = [ "<Shift><Super>5" ];
                move-to-workspace-6 = [ "<Shift><Super>6" ];
                move-to-workspace-7 = [ "<Shift><Super>7" ];
                move-to-workspace-8 = [ "<Shift><Super>8" ];
                move-to-workspace-9 = [ "<Shift><Super>9" ];
                move-to-workspace-0 = [ "<Shift><Super>0" ];
                close = [ "<Super>d" ];
                toggle-fullscreen = [ "<Super>f" ];
                toggle-maximized = [ "<Super>t" ];
                panel-run-dialog = [ "<Super>r" ];
                switch-windows = [
                  "<Super>Tab"
                  "<Alt>Tab"
                  "<Super>e"
                ];
                switch-windows-backward = [
                  "<Super><Shift>Tab"
                  "<Alt><Shift>Tab"
                  "<Super>q"
                ];
                switch-applications = [ ];
                switch-applications-backward = [ ];
                minimize = [ ];
              };
              "org/gnome/nautilus/preferences" = {
                "default-folder-viewer" = "list-view";
              };
              "org/gnome/Weather" = {
                locations = [ bakersfield ];
              };
              "org/gnome/shell/weather" = {
                locations = [ bakersfield ];
              };
              "org/gnome/shell/world-clocks" = {
                locations = [
                  tehran
                  moscow
                  beijing
                ];
              };
              "org/gnome/clocks" = {
                world-clocks = [
                  [
                    (lib.hm.gvariant.mkDictionaryEntry [
                      "location"
                      tehran
                    ])
                  ]
                  [
                    (lib.hm.gvariant.mkDictionaryEntry [
                      "location"
                      moscow
                    ])
                  ]
                  [
                    (lib.hm.gvariant.mkDictionaryEntry [
                      "location"
                      beijing
                    ])
                  ]
                ];
              };
            }
            (generate_custom_keybindings {
              "1" = {
                binding = "<Super><Control>d";
                command = "display-switch.sh -p kestrel-desktop";
                name = "Kestrel display at Desk";
              };
              "2" = {
                binding = "<Super><Control>l";
                command = "display-switch.sh -p kestrel-living";
                name = "Kestrel display in Living Room";
              };
            });
      };
  };
}
