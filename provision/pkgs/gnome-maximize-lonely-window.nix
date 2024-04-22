{ lib, stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation {
  pname = "gnome-maximize-lonely-window";
  version = "git-2023-10-31";

  src = fetchFromGitHub {
    owner = "MrShuster";
    repo = "maximize_lonely_window";
    rev = "c12dacd32754d9ee07373420e8f8797792ec68d7";
    hash = "sha256-qWhmphbn4BRgCFr2p/0X+E0ECPqI21DmJip8icPQwgk=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/gnome-shell/extensions/maximize-lonely-window@MrShuster"
    cp -r * "$out/share/gnome-shell/extensions/maximize-lonely-window@MrShuster"
    runHook postInstall
  '';

  passthru = {
    extensionUuid = "maximize-lonely-window@MrShuster";
    extensionPortalSlug = "maximize-lonely-window";
  };

  meta = with lib; {
    description = "New and maximized windows will be moved to empty workspaces.";
    maintainers = with maintainers; [ MrShuster starr-dusT ];
    homepage = "https://github.com/MrShuster/maximize_lonely_window";
  };
}
