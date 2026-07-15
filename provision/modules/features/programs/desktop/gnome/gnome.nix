{ inputs, ... }:
{
  flake.modules.nixos.gnome =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
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
      imports = with inputs.self.modules.nixos; [
        desktop
        kde-connect
        keyd
        samba-client
        syncthing-tray
      ];

      environment.systemPackages = with pkgs; [
        dconf-editor # Graphical tool for editing settings stored in the dconf database of GNOME
        evolution # Personal information management application that provides email, calendar, and contact management features
        planify # Task manager with Todoist support designed for GNU/Linux
        gnomeExtensions.alphabetical-app-grid # Alphabetically order the app grid and folders
        gnomeExtensions.appindicator # Adds AppIndicator, KStatusNotifierItem and legacy Tray icons support to the Shell
        gnomeExtensions.executor # Execute shell commands display output top bar
        gnomeExtensions.happy-appy-hotkey # Assign hotkeys to applications to give them focus or launch them
        gnome-set-panel-monitor # Set monitor for panel to appear on
        ftw # Build custom ftl wallpapers in a complicated way for no reason
        playerctl # Command-line utility and library for controlling media players that implement MPRIS
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
        displayManager.gdm.enable = true;
        desktopManager.gnome.enable = true;
      };

      services.gnome.gnome-remote-desktop.enable = true;
      networking.firewall.allowedTCPPorts = [ 3389 ];
      systemd.services.gnome-remote-desktop = {
        wantedBy = [ "graphical.target" ];
      };

      # Disable autologin to avoid session conflicts
      services.displayManager.autoLogin.enable = false;
      services.getty.autologinUser = null;
      systemd.targets.sleep.enable = false;
      systemd.targets.suspend.enable = false;
      systemd.targets.hibernate.enable = false;
      systemd.targets.hybrid-sleep.enable = false;

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

      home-manager.users.${config.preferences.user} =
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
                  accent-color = "blue";
                  enable-animations = true;
                  enable-hot-corners = false;
                };
                "org/gnome/desktop/background" = {
                  picture-uri = "file:///home/${config.preferences.user}/.local/share/chezmoi/resources/img/wallpapers/${config.preferences.hostname}.png";
                  picture-uri-dark = "file:///home/${config.preferences.user}/.local/share/chezmoi/resources/img/wallpapers/${config.preferences.hostname}.png";
                };
                "org/gnome/shell" = {
                  favorite-apps = [
                    "chromium-browser.desktop"
                    "com.mitchellh.ghostty.desktop"
                    "org.gnome.Nautilus.desktop"
                    "obsidian.desktop"
                  ];
                  disable-user-extensions = false;
                  enabled-extensions = [
                    "appindicatorsupport@rgcjonas.gmail.com"
                    "executor@raujonas.github.io"
                    "gnome-shell-extension-set-panel-monitor@tstarr.us"
                    "AlphabeticalAppGrid@stuarthayhurst"
                    "happy-appy-hotkey@jqno.nl"
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
                    "variable-refresh-rate"
                  ];
                };
                "org/gnome/mutter/keybindings" = {
                  switch-monitor = [ ];
                  toggle-tiled-left = [ "<Super>h" ];
                  toggle-tiled-right = [ "<Super>l" ];
                };
                "org/gnome/desktop/wm/preferences" = {
                  focus-mode = "sloppy";
                  num-workspaces = 4;
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
                  open-new-window-application-1 = [ "<Shift><Super>1" ];
                  open-new-window-application-2 = [ "<Shift><Super>2" ];
                  open-new-window-application-3 = [ "<Shift><Super>3" ];
                  open-new-window-application-4 = [ "<Shift><Super>4" ];
                  open-new-window-application-5 = [ ];
                  open-new-window-application-6 = [ ];
                  open-new-window-application-7 = [ ];
                  open-new-window-application-8 = [ ];
                  open-new-window-application-9 = [ ];
                  toggle-application-view = [ ];
                  toggle-quick-settings = [ "<Super>z" ];
                  toggle-message-tray = [ "<Super>v" ];
                };
                "org/gnome/settings-daemon/plugins/media-keys" = {
                  play = [ "<Super>p" ];
                  screensaver = [ "<Super>Space" ];
                };
                "org/gnome/settings-daemon/plugins/power" = {
                  sleep-inactive-ac-type = "nothing";
                  sleep-inactive-battery-type = "suspend";
                  sleep-inactive-ac-timeout = 0;
                  sleep-inactive-battery-timeout = 15;
                };
                "org/gnome/desktop/wm/keybindings" = {
                  switch-to-workspace-1 = [ "<Super>q" ];
                  switch-to-workspace-2 = [ "<Super>w" ];
                  switch-to-workspace-3 = [ "<Super>e" ];
                  switch-to-workspace-4 = [ "<Super>r" ];
                  move-to-workspace-1 = [ "<Shift><Super>q" ];
                  move-to-workspace-2 = [ "<Shift><Super>w" ];
                  move-to-workspace-3 = [ "<Shift><Super>e" ];
                  move-to-workspace-4 = [ "<Shift><Super>r" ];
                  close = [ "<Super>d" ];
                  toggle-fullscreen = [ "<Super>f" ];
                  toggle-maximized = [
                    "<Super>t"
                    "<Super>k"
                    "<Super>j"
                  ];
                  move-to-monitor-right = [ "<Shift><Super>l" ];
                  move-to-monitor-left = [ "<Shift><Super>h" ];
                  panel-run-dialog = [ "<Super>x" ];
                  toggle-on-all-workspaces = [ "<Super>s" ];
                  toggle-above = [ "<Super>a" ];
                  switch-windows = [
                    "<Super>Tab"
                    "<Alt>Tab"
                  ];
                  switch-windows-backward = [
                    "<Super><Shift>Tab"
                    "<Alt><Shift>Tab"
                  ];
                  switch-applications = [ ];
                  switch-applications-backward = [ ];
                  switch-group = [ ];
                  switch-group-backward = [ ];
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
                # Appindicator settings
                "org/gnome/shell/extensions/appindicator" = {
                  tray-pos = "left";
                };
                # Happy Appy settings
                "org/gnome/shell/extensions/happy-appy-hotkey" = {
                  app-0 = "Chromium";
                  hotkey-0 = [ "<Super>1" ];
                  app-1 = "Ghostty";
                  hotkey-1 = [ "<Super>2" ];
                  app-2 = "Files";
                  hotkey-2 = [ "<Super>3" ];
                  app-3 = "Obsidian";
                  hotkey-3 = [ "<Super>4" ];
                  app-4 = "Discord";
                  hotkey-4 = [ "<Super>c" ];
                  app-5 = "Mattermost";
                  hotkey-5 = [ "<Super>m" ];
                  hotkey-unbound-cycle = [ "<Super>grave" ];
                  number = 6;
                  restrict-to-current-workspace = true;
                };
                # Executor settings
                "org/gnome/shell/extensions/executor" = {
                  "click-on-output-active" = false;
                  "center-active" = false;
                  "left-active" = false;
                  "right-active" = true;
                  "right-commands-json" = ''
                    {"commands":[
                        {
                          "isActive": true,
                          "command": "echo \" $(hostname) \"",
                          "interval": 300,
                          "uuid": "732cd6de-ff5f-46a7-b8bb-51d1c621cc60"
                        },
                        {
                          "isActive":true,
                          "command":"echo \" $(subwoofer-volume.sh -r)\"",
                          "interval":5,
                          "uuid":"732cd6de-ff5f-46a7-b8bb-51d1c621cc62"
                        }
                      ]
                    }'';
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
