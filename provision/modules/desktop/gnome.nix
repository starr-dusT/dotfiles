{ config, lib, pkgs, user, home-manager, ... }:

let 
  cfg = config.modules.desktop.gnome;
  inherit (builtins) attrNames map;
  inherit (lib.attrsets) mapAttrs' nameValuePair;
  generate_custom_keybindings = binds:
    {
      "org/gnome/settings-daemon/plugins/media-keys" = {
        custom-keybindings = map (name:
          "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/${name}/")
          (attrNames binds);
      };
    } // mapAttrs' (name:
      nameValuePair
      "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/${name}")
    binds;
in {

  options.modules.desktop.gnome = with lib; {
    enable = lib.mkEnableOption "gnome";
    wallpaper = lib.mkOption {
      type = with types; str;
      default = "file://${../../../resources/img/wallpapers/blank.png}";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dconf-editor # Graphical tool for editing settings stored in the dconf database of GNOME.
      gnome-tweaks # Utility for customizing various aspects of the GNOME desktop environment.
      evolution # Personal information management application that provides email, calendar, and contact management features.
      gnomeExtensions.focus-changer # GNOME Shell extension for changing window focus behavior.
      gnomeExtensions.custom-hot-corners-extended # GNOME Shell extension for changing window focus behavior.
      gnome-fullscreen-to-empty-workspace
      gnome-set-panel-monitor
    ];

    environment.gnome.excludePackages = with pkgs; [
      baobab # Disk usage analyzer for the GNOME desktop environment.
      cheese # Webcam application for taking photos and videos.
      epiphany # Web browser for the GNOME desktop environment.
      pkgs.gedit # Text editor for the GNOME desktop environment.
      simple-scan # Simple scanning utility for scanning documents and images.
      totem # Movie player for the GNOME desktop environment.
      yelp # Help viewer application for the GNOME desktop environment.
      evince # Document viewer for the GNOME desktop environment.
      geary # Email client for the GNOME desktop environment.
      seahorse # GNOME application for managing encryption keys and passwords.
      gnome-tour # Guided tour application for introducing users to GNOME desktop environment features.
      snapshot # Utility for taking and managing system snapshots in the GNOME desktop environment.
      gnome-connections # GNOME application for accessing remote machines and services.
      gnome-font-viewer # Utility for previewing and managing fonts in the GNOME desktop environment.
      gnome.gnome-logs # Log viewer application for GNOME.
      gnome.gnome-maps # Map application for the GNOME desktop environment.
      gnome.gnome-music # Music player and management application for GNOME.
      gnome.gnome-shell-extensions # Extensions for enhancing functionality and customization in the GNOME.
    ];

    # Valent for remote control
    programs.kdeconnect = {
      enable = true;
      package = pkgs.valent;
    };
    networking.firewall = rec {
      allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
      allowedUDPPortRanges = allowedTCPPortRanges;
    };

    # Enable wayland gnome
    services.xserver = {
      enable = true;  
      displayManager.gdm = {
        enable = true;  
        wayland = true; 
      };
      desktopManager.gnome.enable = true; 
    };

    # Enable sound and handle conflict (https://github.com/Jovian-Experiments/Jovian-NixOS/issues/99)
    hardware.pulseaudio.enable = lib.mkForce false;

    xdg.mime = {
      enable = true;
      defaultApplications = {    
        "text/plain" = "org.gnome.TextEditor.desktop";

        # Images
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

    home-manager.users.${user} = {
      # Remove unwanted desktop entries
      # Some are added to ~/.local/share/applications and must be removed manually there
      # TODO: Use chezmoi to Hide these desktop files
      xdg.desktopEntries.cups = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.writer = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.math = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.calc = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.draw = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.impress = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.base = { name = ""; exec = null; settings.Hidden = "true"; };
      xdg.desktopEntries.xterm = { name = ""; exec = null; settings.Hidden = "true"; };

      # GNOME settings through home
      dconf.settings = {
        "org/gnome/desktop/interface" = {
            color-scheme = "prefer-dark";
        };
        "org/gnome/desktop/background" = {
          picture-options = "zoom";
          picture-uri = "${cfg.wallpaper}";
          picture-uri-dark = "${cfg.wallpaper}";
        };
        "org/gnome/shell" = {
          favorite-apps = [
            "google-chrome.desktop"
            "kitty.desktop"
            "steam.desktop"
            "discord.desktop"
          ];
          disable-user-extensions = false;
          enabled-extensions = [
            "focus-changer@heartmire"
            "fullscreen-to-empty-workspace@aiono.dev"
            "gnome-set-panel-monitor@tstarr.us"
            "custom-hot-corners-extended@G-dH.github.com"
          ];
        };
        "org/gnome/shell/app-switcher" = {
          current-workspace-only = true;
        };
        "org/gnome/mutter" = {
          overlay-key = "Super";
          center-new-windows = true;
          focus-change-on-pointer-rest = false;
          workspaces-only-on-primary = true;
          dynamic-workspaces = false;
        };
        "org/gnome/desktop/wm/preferences" = {
          auto-raise = false;
          raise-on-click = false;
          num-workspaces = 10;
        };
        "org/gnome/shell/keybindings" = {
          switch-to-application-1 = [];
          switch-to-application-2 = [];
          switch-to-application-3 = [];
          switch-to-application-4 = [];
          switch-to-application-5 = [];
          switch-to-application-6 = [];
          switch-to-application-7 = [];
          switch-to-application-8 = [];
          switch-to-application-9 = [];
          toggle-quick-settings = [];
        };
        "org/gnome/settings-daemon/plugins/media-keys" = {
          screensaver = [];    
        };
        "org/gnome/desktop/wm/keybindings" = {
          switch-to-workspace-1 = ["<Super>1"];            
          switch-to-workspace-2 = ["<Super>2"];            
          switch-to-workspace-3 = ["<Super>3"];            
          switch-to-workspace-4 = ["<Super>4"];            
          switch-to-workspace-5 = ["<Super>5"];            
          switch-to-workspace-6 = ["<Super>6"];            
          switch-to-workspace-7 = ["<Super>7"];            
          switch-to-workspace-8 = ["<Super>8"];            
          switch-to-workspace-9 = ["<Super>9"];            
          switch-to-workspace-0 = ["<Super>0"];            
          move-to-workspace-1 = ["<Shift><Super>1"];       
          move-to-workspace-2 = ["<Shift><Super>2"];       
          move-to-workspace-3 = ["<Shift><Super>3"];       
          move-to-workspace-4 = ["<Shift><Super>4"];       
          move-to-workspace-5 = ["<Shift><Super>5"];       
          move-to-workspace-6 = ["<Shift><Super>6"];       
          move-to-workspace-7 = ["<Shift><Super>7"];       
          move-to-workspace-8 = ["<Shift><Super>8"];       
          move-to-workspace-9 = ["<Shift><Super>9"];       
          move-to-workspace-0 = ["<Shift><Super>0"];       
          move-to-monitor-left = ["<Shift><Super>h"];      
          move-to-monitor-right = ["<Shift><Super>l"];     
          close = ["<Super>d"];                            
          toggle-fullscreen = [ "<Super>f" ];              
          toggle-maximized = [ "<Super>t" ];               
          raise-or-lower = [ "<Super>s" ];                 
          switch-windows = ["<Super>Tab"];                 
          switch-windows-backward = ["<Shift><Super>Tab"]; 
          minimize = [];
        };
        # custom-hot-corners-extended configs
        "org/gnome/shell/extensions/custom-hot-corners-extended/misc" = {
            panel-menu-enable = false;
        };
        "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-left-1" = {
            action = "toggle-overview";
        };
        "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-left-4" = {
            action = "next-workspace";
        };
        "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-left-5" = {
            action = "prev-workspace";
        };
      } // generate_custom_keybindings {
        "terminal" = { binding = "<Super><Control>Return"; command = "kitty"; name = "Open Terminal"; };
        "browser" = { binding = "<Super><Control>b"; command = "google-chrome-stable --simulate-outdated-no-au='Tue, 31 Dec 2099 23:59:59 GMT'"; name = "Open Browser"; };
        "steam" = { binding = "<Super><Control>s"; command = "steam"; name = "Open Steam"; };
        "discord" = { binding = "<Super><Control>d"; command = "discord"; name = "Open Discord"; };
        "nautilus" = { binding = "<Super><Control>f"; command = "nautilus"; name = "Open File Manager"; };
      };
    };
  };
}
