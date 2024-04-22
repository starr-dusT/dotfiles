{ lib, stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation {
  pname = "gnome-set-panel-monitor";
  version = "git-2024-04-11";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "gnome-set-panel-monitor";
    rev = "b1dadb63c153cb9e1c546eb1d151b169d4c05e39";
    hash = "sha256-iHLd4TYgpTxwrPnfx80H+JGV8SpP1wrGgRPPnMyaUaA=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/gnome-shell/extensions/gnome-set-panel-monitor@tstarr.us"
    cp -r * "$out/share/gnome-shell/extensions/gnome-set-panel-monitor@tstarr.us"
    runHook postInstall
  '';

  passthru = {
    extensionUuid = "gnome-set-panel-monitor@tstarr.us";
    extensionPortalSlug = "gnome-set-panel-monitor";
  };

  meta = with lib; {
    description = "Set monitor for panel to appear on.";
    maintainers = with maintainers; [ starr-dusT ];
    homepage = "https://github.com/starr-dusT/gnome-set-panel-monitor";
  };
}
