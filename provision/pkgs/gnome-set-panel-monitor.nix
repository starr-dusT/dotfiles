{
  lib,
  stdenv,
  fetchFromGitHub,
}:

stdenv.mkDerivation {
  pname = "gnome-set-panel-monitor";
  version = "git-2026-05-25";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "gnome-set-panel-monitor";
    rev = "24d307c9053dec9829eae88005eb0c0986f037dc";
    hash = "sha256-wek8WyYj7C01NhtdHDQwUDsiwBQVnY02602Vli5w+k4=";
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
