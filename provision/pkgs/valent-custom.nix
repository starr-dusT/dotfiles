{
  lib,
  stdenv,
  fetchFromGitHub,
  desktop-file-utils,
  meson,
  ninja,
  pkg-config,
  wrapGAppsHook4,
  vala,
  evolution-data-server-gtk4,
  gdk-pixbuf,
  glib,
  glib-networking,
  gobject-introspection,
  gnutls,
  gst_all_1,
  json-glib,
  libadwaita,
  libdex,
  libpeas2,
  libphonenumber,
  libportal-gtk4,
  pipewire,
  pulseaudio,
  tinysparql,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "valent-custom";
  version = "1.0.0.alpha.50-master";

  src = fetchFromGitHub {
    owner = "andyholmes";
    repo = "valent";
    rev = "df82168bc37ad1ec700c66b0f0f5dfd7a07be485";
    hash = "sha256-bg5p7Juw+O2vrfi2uDA69NPy68Zu8ig4ycVjhGkQ4ps=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [
    desktop-file-utils
    meson
    ninja
    pkg-config
    wrapGAppsHook4
    vala
  ];

  buildInputs = [
    evolution-data-server-gtk4
    gdk-pixbuf
    glib
    glib-networking
    gnutls
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gobject-introspection
    json-glib
    libadwaita
    libdex
    libpeas2
    libphonenumber
    libportal-gtk4
    pipewire
    pulseaudio
    tinysparql
  ];

  mesonFlags = [
    (lib.mesonBool "plugin_bluez" true)
  ];

  meta = {
    description = "Implementation of the KDE Connect protocol, built on GNOME platform libraries";
    mainProgram = "valent";
    longDescription = ''
      Note that you have to open firewall ports for other devices
      to connect to it. Use either:
      ```nix
      programs.kdeconnect = {
        enable = true;
        package = pkgs.valent;
      }
      ```
      or open corresponding firewall ports directly:
      ```nix
      networking.firewall = rec {
        allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
        allowedUDPPortRanges = allowedTCPPortRanges;
      }
      ```
    '';
    homepage = "https://valent.andyholmes.ca";
    changelog = "https://github.com/andyholmes/valent/blob/${finalAttrs.src.rev}/CHANGELOG.md";
    license = with lib.licenses; [
      gpl3Plus
      cc0
      cc-by-sa-30
    ];
    maintainers = with lib.maintainers; [ aleksana ];
    platforms = lib.platforms.linux;
  };
})
