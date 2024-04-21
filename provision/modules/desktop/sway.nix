{ config, lib, pkgs, user, inputs, ... }:

let 
    cfg = config.modules.desktop.sway;

    # currently, there is some friction between sway and gtk:
    # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
    # the suggested way to set gtk settings is with gsettings
    # for gsettings to work, we need to tell it where the schemas are
    # using the XDG_DATA_DIR environment variable
    # run at the end of sway config
    configure-gtk = pkgs.writeTextFile {
        name = "configure-gtk";
        destination = "/bin/configure-gtk";
        executable = true;
        text = let
          schema = pkgs.gsettings-desktop-schemas;
          datadir = "${schema}/share/gsettings-schemas/${schema.name}";
        in ''
          export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
          gnome_schema=org.gnome.desktop.interface
          gsettings set $gnome_schema gtk-theme 'Dracula'
          '';
    };

in {
  options.modules.desktop.sway.enable = lib.mkEnableOption "sway";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [
      sway # Tiling Wayland compositor and a drop-in replacement for the i3 window manager for X11.
      swayidle # Idle manager for Wayland, executing actions when the system is idle.
      swaybg # Wallpaper utility for Wayland, setting the background image.
      sway-scratchpad # Helper tool for managing scratchpad windows in the Sway window manager.
      grim # Screenshot utility for Wayland.
      mako # Lightweight notification daemon for Wayland.
      libnotify # Library for sending desktop notifications.
      wdisplays # Utility for managing displays in a Wayland session.
      playerctl # Command-line utility for controlling media players.
      wayland # Protocol for a compositor to talk to its clients as well as a C library implementation of that protocol.
      xwayland # X server running as a Wayland client.
      configure-gtk # GTK-based utility for configuring various aspects of the desktop environment.
      xdg-utils # Collection of tools for managing desktop environments based on the XDG specifications.
      glib # Library providing various core functions for the GNOME project.
      dracula-theme # Dark theme for various applications and environments.
      gnome3.adwaita-icon-theme # Default icon theme for GNOME.
      networkmanagerapplet # GNOME applet for NetworkManager.
      pcmanfm # Lightweight file manager for X11.
      udiskie # Removable disk automounter for udisks.
      pavucontrol # GTK-based volume control utility for PulseAudio.
      waybar # Highly customizable Wayland bar for Sway and Wlroots-based compositors.
      (pkgs.waybar.overrideAttrs (oldAttrs: {
         mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
         })
      )
    ] ++ [
      inputs.hyprland-contrib.packages.${pkgs.system}.grimblast # Hyprland version of Grimshot
    ];

    xdg = {
      portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
    };

    services = {
      gvfs.enable = true;
      blueman.enable = true;
      printing.enable = true;
      printing.drivers = [ pkgs.hplip ];
      avahi.enable = true;
      avahi.nssmdns4 = true;
    };

    # enable sway window manager
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
    programs.hyprland = {
      package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      enable = true;
      xwayland.enable = true;
    };
  };
}
