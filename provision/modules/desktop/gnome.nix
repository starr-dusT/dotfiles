{ config, lib, pkgs, user, home-manager, ... }:

let cfg = config.modules.desktop.gnome;
in {

  options.modules.desktop.gnome = with lib; {
    enable = lib.mkEnableOption "gnome";
    #privateKeyFile = lib.mkOption { type = with types; str; };
    #address = lib.mkOption { type = with types; listOf str; };
    #publicKey = lib.mkOption { type = with types; str; };
    #endpoint = lib.mkOption { type = with types; str; };
    #autostart = lib.mkOption {
    #  type = with types; bool;
    #  default = false;
    #};
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gnome.dconf-editor
      gnomeExtensions.focus-changer
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

    # GNOME settings through home
    home-manager.users.${user} = {
      dconf.settings = {
        "org/gnome/desktop/background" = {
          picture-options = "centered";
          picture-uri = "file://${../../../resources/img/kestrel.png}";
          picture-uri-dark = "file://${../../../resources/img/kestrel.png}";
        };
        "org/gnome/shell" = {
          favorite-apps = [
            "steam.desktop"
            "discord.desktop"
            "org.gnome.Console.desktop"
            "google-chrome.desktop"
          ];
          disable-user-extensions = false;
          enabled-extensions = [
            "focus-changer@heartmire"
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
      };
    };
  };
}
