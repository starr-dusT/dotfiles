{ lib, stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation {
  pname = "gnome-set-panel-monitor";
  version = "git-2024-08-28";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "gnome-set-panel-monitor";
    rev = "62d1a7e7ed83d56b33e32f1ad948c99a559b6dbd";
    hash = "sha256-NB/DycA/VxIeFQk2l0MW7G5VVdJI/Zemy2l28VZx2Cc=";
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
