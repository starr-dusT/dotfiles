{ lib, stdenv, fetchFromGitHub, glib }:

stdenv.mkDerivation {
  pname = "stacks-in-gnome";
  version = "git-2025-01-10";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "gnome-shell-extension-stacks-in-gnome";
    rev = "dbd3edd8134f3331a60f45f44a9bdec4af9beefe";
    hash = "sha256-RbidDs9de2BLxZaPp4TVY6AYGKZKXeLioTHFCcvVF2Q=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/gnome-shell/extensions/stacks-in-gnome@tstarr.us"
    cp -r * "$out/share/gnome-shell/extensions/stacks-in-gnome@tstarr.us"
    runHook postInstall
  '';

  passthru = {
    extensionUuid = "stacks-in-gnome@tstarr.us";
    extensionPortalSlug = "stacks-in-gnome";
  };

  meta = with lib; {
    description = "";
    maintainers = with maintainers; [ starr-dusT ];
    homepage = "https://github.com/starr-dusT/gnome-shell-extension-stacks-in-gnome";
  };
}
