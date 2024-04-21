{ config, lib, pkgs, user, inputs, ... }:
{
  imports = [ ./keyd.nix ./browser.nix ./gnome.nix ./sway.nix ];

  environment.systemPackages = with pkgs; [
    bc
    fzf
    gamemode
    discord
    inkscape
    libreoffice-fresh
    xournal
    mpv
    p7zip
    borgbackup
    gimp
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
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
