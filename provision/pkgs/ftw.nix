{ lib, stdenv, fetchFromGitHub, python312Packages }:

python312Packages.buildPythonApplication rec {
  pname = "ftw";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "ftw";
    rev = "567125b8db098ade1c792fa3dafd9f099273f804";
    sha256 = "sha256-6gOYyGWNJRRCylmSVrwwNTQMHmVrV8SQ0muXeqzVLTY=";
  };

  format = "pyproject";

  propagatedBuildInputs = with python312Packages; [
    numpy
    wand
    setuptools
  ];

  # There are no tests
  doCheck = false;

  postInstall = ''
    cp -R "$src/ftw/res" "$out/lib/python3.12/site-packages/ftw"
  '';

  meta = with lib; {
    homepage = "https://github.com/starr-dusT/ftw";
    description = "Build custom ftl wallpapers in a complicated way for no reason";
    mainProgram = "ftw";
    maintainers = [ maintainers.starr-dusT ];
  };
}
