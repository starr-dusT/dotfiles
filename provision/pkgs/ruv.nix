{
  stdenv,
  fetchFromGitHub,
  python3,
}:

stdenv.mkDerivation {
  pname = "ruv";
  version = "git-2026-10-04";

  src = fetchFromGitHub {
    owner = "svenlange2";
    repo = "Ryzen-5800x3d-linux-undervolting";
    rev = "c08815d1eca6f63585e8ef8f711706e75d5739aa";
    hash = "sha256-Ec50QDNkBYNbAVUrUr6Ac4G6MDIkBdXrjSJ2skMlulc=";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp ruv.py $out/bin/ruv
    substituteInPlace $out/bin/ruv \
      --replace-fail "#!/bin/python" "#!${python3}/bin/python3"
    chmod +x $out/bin/ruv
  '';
}
