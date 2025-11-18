{
  lib,
  stdenv,
  fetchFromGitHub,
}:

stdenv.mkDerivation {
  pname = "gnome-set-panel-monitor";
  version = "git-2026-11-17";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "gnome-set-panel-monitor";
    rev = "c3403d65bdbe22a4a8db564ef43a89f95d4bf2fa";
    hash = "sha256-ZBgl3iEsmA/TU8K2R9jFm/H/Ss7S5GyKZF5S6SzttCs=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/gnome-shell/extensions/gnome-shell-extension-set-panel-monitor@tstarr.us"
    cp -r * "$out/share/gnome-shell/extensions/gnome-shell-extension-set-panel-monitor@tstarr.us"
    runHook postInstall
  '';

  passthru = {
    extensionUuid = "gnome-shell-extension-set-panel-monitor@tstarr.us";
    extensionPortalSlug = "gnome-shell-extension-set-panel-monitor@tstarr.us";
  };

  meta = with lib; {
    description = "Set monitor for panel to appear on.";
    maintainers = with maintainers; [ starr-dusT ];
    homepage = "https://github.com/starr-dusT/gnome-set-panel-monitor";
  };
}
