{ lib, stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation {
  pname = "gnome-fullscreen-to-new-workspace";
  version = "git-2023-08-19";

  src = fetchFromGitHub {
    owner = "onsah";
    repo = "fullscreen-to-new-workspace";
    rev = "26014b6f8d569891381ab2ebc75c74a51d2454df";
    hash = "sha256-S9mvODQqOBCmA+L1TBJm/c1v5Y+qVKuFpQf799LEwcI=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/gnome-shell/extensions/fullscreen-to-empty-workspace@aiono.dev"
    cp -r src/* "$out/share/gnome-shell/extensions/fullscreen-to-empty-workspace@aiono.dev"
    runHook postInstall
  '';

  passthru = {
    extensionUuid = "fullscreen-to-empty-workspace@aiono.dev";
    extensionPortalSlug = "fullscreen-to-empty-workspace";
  };

  meta = with lib; {
    description = "New, maximized and fullscreen windows will be moved to empty workspaces.";
    maintainers = with maintainers; [ onsah starr-dusT ];
    homepage = "https://github.com/onsah/fullscreen-to-new-workspace";
  };
}
