{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "rss-bridge";
  version = "af9996ce3462b5b1ee0a8e6d95b5b010fc26b0a5";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "rss-bridge";
    rev = version;
    sha256 = "sha256-e1VstKjKfnNmjYX0k2FswGF0Kj+AOOEcRqcGw6yyRB8=";
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
    homepage = "https://github.com/starr-dusT/rss-bridge";
    license = licenses.unlicense;
    maintainers = with maintainers; [ starr-dusT ];
    platforms = platforms.all;
  };
}
