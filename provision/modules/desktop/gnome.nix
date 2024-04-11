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
      gnome.dconf-editor
      gnome.gnome-tweaks
      gnomeExtensions.focus-changer
      # TODO: Add back when fixed upstream
      #gnomeExtensions.fullscreen-avoider
    ];

    environment.gnome.excludePackages = with pkgs.gnome; [
      baobab          # disk usage analyzer
      cheese          # photo booth
      epiphany        # web browser
      pkgs.gedit      # text editor
      simple-scan     # document scanner
      totem           # video player
      yelp            # help viewer
      evince          # document viewer
      geary           # email client
      seahorse        # password manager
      pkgs.gnome-tour # tour app
      pkgs.snapshot   # camera application

      # these should be self explanatory
      pkgs.gnome-connections
      gnome-font-viewer 
      gnome-logs 
      gnome-maps 
      gnome-music 
    ];

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
    sound.enable = true;
    hardware.pulseaudio.enable = lib.mkForce false;

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
        "org/gnome/desktop/background" = {
          picture-options = "centered";
          picture-uri = "${cfg.wallpaper}";
          picture-uri-dark = "${cfg.wallpaper}";
        };
        "org/gnome/shell" = {
          favorite-apps = [
            "google-chrome.desktop"
            "org.gnome.Console.desktop"
            "steam.desktop"
            "discord.desktop"
          ];
          disable-user-extensions = false;
          enabled-extensions = [
            "focus-changer@heartmire"
            "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
            "fullscreen-avoider@noobsai.github.com"
          ];
        };
        "org/gnome/mutter" = {
          center-new-windows = true;
          focus-change-on-pointer-rest = false;
          overlay-key = "Super";
          workspaces-only-on-primary = true;
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
          switch-to-workspace-1 = ["<Super>1"];            # 
          switch-to-workspace-2 = ["<Super>2"];            # 
          switch-to-workspace-3 = ["<Super>3"];            # 
          switch-to-workspace-4 = ["<Super>4"];            # 
          move-to-workspace-1 = ["<Shift><Super>1"];       # 
          move-to-workspace-2 = ["<Shift><Super>2"];       # 
          move-to-workspace-3 = ["<Shift><Super>3"];       # 
          move-to-workspace-4 = ["<Shift><Super>4"];       # 
          move-to-monitor-left = ["<Shift><Super>h"];      # 
          move-to-monitor-right = ["<Shift><Super>l"];     # 
          close = ["<Super>d"];                            # 
          toggle-fullscreen = [ "<Super>f" ];              # 
          toggle-maximized = [ "<Super>t" ];               # 
          raise-or-lower = [ "<Super>s" ];                 # 
          switch-windows = ["<Super>Tab"];                 # 
          switch-windows-backward = ["<Shift><Super>Tab"]; # 
          minimize = [];
        };
      } // generate_custom_keybindings {
        "terminal" = { binding = "<Super>Return"; command = "kgx"; name = "Open Terminal"; };
        "browser" = { binding = "<Super><Control>b"; command = "google-chrome-stable --simulate-outdated-no-au='Tue, 31 Dec 2099 23:59:59 GMT'"; name = "Open Browser"; };
      };
    };
  };
}
