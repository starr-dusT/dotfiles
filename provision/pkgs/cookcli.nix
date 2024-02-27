{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, openssl
, stdenv
}:

rustPlatform.buildRustPackage rec {
  pname = "cookcli";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "cooklang";
    repo = "cookcli";
    rev = "v${version}";
    hash = "sha256-43++90St6XxUn7fhCooH+YEZhA2eTJcPVASaDMOZ8EU=";
  };

  cargoHash = "sha256-2Pr6td6TpXcUna2GmLHMDKxLFsWN3LM72/PvFyFOHw0=";

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    openssl
  ];

  env = {
    OPENSSL_NO_VENDOR = true;
  };

  meta = with lib; {
    description = "Command line program which provides a suite of tools to create shopping lists and maintain recipes";
    homepage = "https://github.com/cooklang/cookcli.git";
    changelog = "https://github.com/cooklang/cookcli/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "cookcli";
  };
}
