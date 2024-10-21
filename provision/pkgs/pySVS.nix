{ lib, stdenv, fetchFromGitHub, python3Packages }:

python3Packages.buildPythonApplication rec {
  pname = "pySVS";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "pySVS";
    rev = "5c483d96c58aab77a18b64f3003dd67be584977c";
    sha256 = "sha256-2j4bIGmuQOBuRAy4s6M56USbrJ2SGK3cFYc3AkQ6lKg=";
  };

  format = "pyproject";

  propagatedBuildInputs = with python3Packages; [
    requests
    bleak
    pillow
    setuptools
  ];

  # There are no tests
  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/starr-dusT/pySVS";
    description = "Python CLI for controlling SVS subwoofers over bluetooth.";
    mainProgram = "pySVS";
    license = licenses.mit;
    maintainers = [ maintainers.starr-dusT ];
  };
}
