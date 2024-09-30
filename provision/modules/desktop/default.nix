{ config, lib, pkgs, user, inputs, ... }:
{
  imports = [ ./browser ./keyd.nix ./gnome.nix ];

  environment.systemPackages = with pkgs; [
    bc # Arbitrary-precision arithmetic language.
    fzf # Command-line fuzzy finder for Unix-like operating systems.
    gamemode # Optimizes system performance for gaming by adjusting system settings.
    vesktop # Voice, video, and text communication platform for communities and friends.
    inkscape # Vector graphics editor for creating illustrations, icons, logos, diagrams, and more.
    libreoffice-fresh # Office suite compatible with Microsoft Office.
    xournal # Note-taking and sketching application.
    mpv # Media player for playing audio and video files.
    p7zip # Command-line file archiver with high compression ratio.
    gimp # GNU Image Manipulation Program for editing and composing raster images.
  ];

  # xdg-desktop-portal settings
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
      "application/pdf" = [ "google-chrome.desktop" ]; };
  };
}
