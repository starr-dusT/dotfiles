{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.desktop;
in
{
  imports = [
    ./flatpak.nix
    ./gnome-remote-desktop.nix
    ./gnome.nix
    ./kde-connect.nix
    ./keyd.nix
    ./peripherals.nix
    ./samba-client.nix
    ./scripts
  ];

  options.modules.optional.desktop.enable = lib.mkEnableOption "desktop";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      bc # Arbitrary-precision arithmetic language
      gamemode # Optimizes system performance for gaming by adjusting system settings
      discord # Voice, video, and text communication platform for communities and friends
      inkscape # Vector graphics editor for creating illustrations, icons, logos, diagrams, and more
      libreoffice-fresh # Office suite compatible with Microsoft Office
      mpv # Media player for playing audio and video files
      p7zip # Command-line file archiver with high compression ratio
      gimp # GNU Image Manipulation Program for editing and composing raster images
      libnotify # Library that sends desktop notifications to a notification daemon
    ];

    services.flatpak = {
      enable = true;
      update.onActivation = true;
    };

    services.dbus.enable = true;
    xdg.mime = {
      enable = true;
      defaultApplications = {
        "inode/directory" = [ "pcmanfm.desktop" ];
        "text/html" = [ "google-chrome.desktop" ];
        "x-scheme-handler/http" = [ "google-chrome.desktop" ];
        "x-scheme-handler/https" = [ "google-chrome.desktop" ];
        "x-scheme-handler/ftp" = [ "google-chrome.desktop" ];
        "x-scheme-handler/chrome" = [ "google-chrome.desktop" ];
        "x-scheme-handler/about" = [ "google-chrome.desktop" ];
        "x-scheme-handler/unknown" = [ "google-chrome.desktop" ];
        "application/x-extension-htm" = [ "google-chrome.desktop" ];
        "application/x-extension-html" = [ "google-chrome.desktop" ];
        "application/x-extension-shtml" = [ "google-chrome.desktop" ];
        "application/xhtml+xml" = [ "google-chrome.desktop" ];
        "application/x-extension-xhtml" = [ "google-chrome.desktop" ];
        "application/x-extension-xht" = [ "google-chrome.desktop" ];
        "application/pdf" = [ "google-chrome.desktop" ];
      };
    };

    modules = {
      optional = {
        programs = {
          chrome.enable = true;
          firefox.enable = true;
        };
      };
    };
  };
}
