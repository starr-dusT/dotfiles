{ config, lib, pkgs, pkgs-unstable, user, jovian-nixos, home-manager, ... }:
{
  imports = [
    (jovian-nixos + "/modules")
    home-manager.nixosModule
  ];

    jovian = {
      steam.desktopSession = "gnome-xorg";
      steam.enable = true;
      steam.autoStart = true; 
      steam.user = "${user}";
      devices.steamdeck = {
        enable = true;
      };
    };

    services.xserver.enable = true;
    services.xserver.displayManager.gdm.enable = false;
    services.xserver.desktopManager.gnome.enable = true;

    # Enable GNOME
    sound.enable = true;
    hardware.pulseaudio.enable = lib.mkForce false;

    environment.systemPackages = with pkgs; [
      gnome.gnome-terminal
      gnomeExtensions.dash-to-dock
      jupiter-dock-updater-bin
      steamdeck-firmware
    ];

    # GNOME settings through home
    home-manager.users.${user} = {
      dconf.settings = {
        # Enable on-screen keyboard
        "org/gnome/desktop/a11y/applications" = {
          screen-keyboard-enabled = true;
        };
        "org/gnome/shell" = {
          enabled-extensions = [
            "dash-to-dock@micxgx.gmail.com"
          ];
          favorite-apps = ["steam.desktop"];
        };
        # Dash to Dock settings for a better touch screen experience
        "org/gnome/shell/extensions/dash-to-dock" = {
          background-opacity = 0.80000000000000004;
          custom-theme-shrink = true;
          dash-max-icon-size = 48;
          dock-fixed = true;
          dock-position = "LEFT";
          extend-height = true;
          height-fraction = 0.60999999999999999;
          hot-keys = false;
          preferred-monitor = -2;
          preferred-monitor-by-connector = "eDP-1";
          scroll-to-focused-application = true;
          show-apps-at-top = true;
          show-mounts = true;
          show-show-apps-button = true;
          show-trash = false;
        };
      };
    };
  };
}
