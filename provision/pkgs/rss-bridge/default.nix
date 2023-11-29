{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "rss-bridge";
  version = "b037d1b4d1f0b0f422e21125ddef00a58e185ed1";

  src = fetchFromGitHub {
    owner = "RSS-Bridge";
    repo = "rss-bridge";
    rev = version;
    sha256 = "sha256-zyWnjSYE2NFK/OJLnsFsE5oEyf+yrJe8TT6MH4roBwU=";
  };

  patches = [
    ./paths.patch
  ];

  installPhase = ''
    mkdir $out/
    cp -R ./* $out
  '';

  meta = with lib; {
    description = "The RSS feed for websites missing it";
    homepage = "https://github.com/RSS-Bridge/rss-bridge";
    license = licenses.unlicense;
    maintainers = with maintainers; [ starr-dusT ];
    platforms = platforms.all;
  };
}
