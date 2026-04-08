{ inputs, ... }:
{
  flake.modules.nixos.desktop =
    { pkgs, config, ... }:
    {
      imports = with inputs.self.modules.nixos; [
        chrome
        firefox
        flatpak
        desktop-scripts
      ];

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
        ghostty # Fast, native, feature-rich terminal emulator pushing modern features
        libimobiledevice # Library to support iPhone, iPod Touch and iPad devices on Linux
        ifuse # Fuse filesystem implementation to access the contents of iOS devices
        pySVS # Control SVS subwoofers from the command-line
        simple-scan
      ];

      services.usbmuxd.enable = true; # for iOS mounting as storage
      services.flatpak.packages = [ "com.github.tchx84.Flatseal" ];

      hardware.sane.enable = true;
      users.users.${config.preferences.user}.extraGroups = [
        "scanner"
        "lp"
      ];
      services.printing = {
        enable = true;
        drivers = [
          pkgs.epson-escpr
          pkgs.epson-escpr2
        ];
      };
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
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
    };
}
