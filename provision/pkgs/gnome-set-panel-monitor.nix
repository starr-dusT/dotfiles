{ lib, stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation {
  pname = "gnome-set-panel-monitor";
  version = "git-2025-04-01";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "gnome-set-panel-monitor";
    rev = "9afeb755297c467a3236d07b58ff9feae04ba13d";
    hash = "sha256-EDR7qsiTQag0NytrokCPTUEygvAXfOP9bGT1W+hVkZY=";
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
