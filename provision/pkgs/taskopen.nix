{ lib, stdenv, fetchFromGitHub, nim }:

stdenv.mkDerivation rec {
  pname = "taskopen";
  version = "2.0.0alpha";

  src = fetchFromGitHub {
    owner = "jschlatow";
    repo = "taskopen";
    rev = "12b04b871a4e50d07cabf8574a7347abf54ff43f";
    hash = "sha256-7OxfZ+KfcbrDYFAxi2D/GmHHqoCEmi+t9CeC0DkVhVY=";
  };

  buildInputs = [ nim ];

  buildPhase = ''
    export HOME=$(pwd)/home; mkdir -p $HOME
  '';

  installPhase = ''
    make PREFIX=$out
    make PREFIX=$out install
  '';

  meta = with lib; {
    description = "Wrapper for interaction with taskwarrior annotations.";
    homepage = "https://github.com/jschlatow/taskopen";
    license = [ licenses.gpl2 ];
    maintainers = [ maintainers.starr-dusT ];
  };
}
