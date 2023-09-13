{ config, lib, pkgs, pkgs-unstable, user, ... }:

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
      alacritty
      sway
      swayidle
      swaybg
      sway-scratchpad
      grim # screenshot functionality
      wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
      mako # notification system developed by swaywm maintainer
      wdisplays # tool to configure displays
      playerctl
      wayland
      xwayland
      configure-gtk
      xdg-utils # for opening default programs when clicking links
      glib # gsettings
      dracula-theme # gtk theme
      gnome3.adwaita-icon-theme  # default gnome cursors
      bc
      fzf

      # From home config
      networkmanagerapplet
      pcmanfm
      google-chrome
      gamemode
      discord
      inkscape
      libreoffice-fresh
      mpv
      udiskie
      p7zip
      pavucontrol
    ] ++ [
      pkgs-unstable.brave
    ];

    # xdg-desktop-portal works by exposing a series of D-Bus interfaces
    # known as portals under a well-known name
    # (org.freedesktop.portal.Desktop) and object path
    # (/org/freedesktop/portal/desktop).
    # The portal interfaces include APIs for file access, opening URIs,
    # printing and others.
    services.dbus.enable = true;
    xdg = {
      portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
      mime = {
        enable = true;
        defaultApplications = {
          "text/plain" = [ "nvim.desktop" ];
          "inode/directory" = [ "pcmanfm.desktop" ];
          "text/html" = [ "brave-browser.desktop" ];
          "x-scheme-handler/http" = [ "brave-browser.desktop" ];
          "x-scheme-handler/https" = [ "brave-browser.desktop" ];
          "x-scheme-handler/ftp" = [ "brave-browser.desktop" ];
          "x-scheme-handler/chrome" = [ "brave-browser.desktop" ];
          "x-scheme-handler/about" = [ "brave-browser.desktop" ];
          "x-scheme-handler/unknown" = [ "brave-browser.desktop" ];
          "application/x-extension-htm" = [ "brave-browser.desktop" ];
          "application/x-extension-html" = [ "brave-browser.desktop" ];
          "application/x-extension-shtml" = [ "brave-browser.desktop" ];
          "application/xhtml+xml" = [ "brave-browser.desktop" ];
          "application/x-extension-xhtml" = [ "brave-browser.desktop" ];
          "application/x-extension-xht" = [ "brave-browser.desktop" ];
          "application/pdf" = [ "brave-browser.desktop" ];
        };
      };
    };

    # enable sway window manager
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
  };
}
