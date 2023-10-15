{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "sway-scratchpad";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "matejc";
    repo = "sway-scratchpad";
    rev = "2f4d39927a154108f193771a90ae7f298e7ae65f";
    hash = "sha256-TgiwU95BbKFvISDe8+wn8VQ+8vQNuGsb+7p9kP//5lw=";
  };

  cargoHash = "sha256-VLWjpb9OmYkBykP51YURWnvgzI1DW0731DbDcJh/7h8=";

  meta = with lib; {
    description = "Convert a command to a scratchpad, and toggle visibility with sway/i3.";
    homepage = "https://github.com/matejc/sway-scratchpad";
    license = licenses.bsd2;
    maintainers = [ maintainers.starr-dusT ];
  };
}
