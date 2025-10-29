{
  lib,
  fetchFromGitHub,
  python312Packages,
}:

python312Packages.buildPythonApplication {
  pname = "ftw";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "starr-dusT";
    repo = "ftw";
    rev = "125419007f66d9528c6278873cc28840c754b6ee";
    sha256 = "sha256-Z2dsSgIKOr1l8vWN5gI9ZsEdTnnQVH6cG8X32TYekSU=";
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
