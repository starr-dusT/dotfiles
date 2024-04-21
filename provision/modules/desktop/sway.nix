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
      sway
      swayidle
      swaybg
      sway-scratchpad
      grim # screenshot functionality
      mako # notification system developed by swaywm maintainer
      libnotify
      wdisplays # tool to configure displays
      playerctl
      wayland
      xwayland
      configure-gtk
      xdg-utils # for opening default programs when clicking links
      glib # gsettings
      dracula-theme # gtk theme
      gnome3.adwaita-icon-theme  # default gnome cursors
      networkmanagerapplet
      pcmanfm
      udiskie
      pavucontrol
      waybar
      (pkgs.waybar.overrideAttrs (oldAttrs: {
         mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
         })
      )
    ] ++ [
      inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
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
