{ lib
, buildDotnetModule
, dotnetCorePackages
, fetchFromGitHub
, libX11
, libgdiplus
, ffmpeg
, openal
, libsoundio
, sndio
, pulseaudio
, vulkan-loader
, glew
, libGL
, libICE
, libSM
, libXcursor
, libXext
, libXi
, libXrandr
, udev
, SDL2
, SDL2_mixer
, fontconfig
}:

buildDotnetModule rec {
  pname = "ryujinx";
  version = "3513"; # commit number

  src = fetchFromGitHub {
    owner = "GreemDev";
    repo = "Ryujinx";
    rev = "6be88380433e04ea8898645ffc0111e1a31eafb1";
    hash = "sha256-t0qqshf2x+wogHtoxj9bthU03h29wvhrFCTUw8C2DDo=";
  };

  enableParallelBuilding = false;

  dotnet-sdk = dotnetCorePackages.sdk_8_0;
  dotnet-runtime = dotnetCorePackages.runtime_8_0;

  nugetDeps = ./deps.nix;

  runtimeDeps = [
    libX11
    libgdiplus
    SDL2_mixer
    openal
    libsoundio
    sndio
    pulseaudio
    vulkan-loader
    ffmpeg
    udev

    # Avalonia UI
    glew
    libICE
    libSM
    libXcursor
    libXext
    libXi
    libXrandr

    # Headless executable
    libGL
    SDL2
    fontconfig
  ];

  projectFile = "Ryujinx.sln";
  testProjectFile = "src/Ryujinx.Tests/Ryujinx.Tests.csproj";
  doCheck = true;

  dotnetFlags = [
    "/p:ExtraDefineConstants=DISABLE_UPDATER%2CFORCE_EXTERNAL_BASE_DIR"
  ];

  executables = [
    "Ryujinx.Headless.SDL2"
    "Ryujinx"
  ];

  makeWrapperArgs = [
    # Without this Ryujinx fails to start on wayland. See https://github.com/Ryujinx/Ryujinx/issues/2714
    "--set SDL_VIDEODRIVER x11"
  ];

  preInstall = ''
    # workaround for https://github.com/Ryujinx/Ryujinx/issues/2349
    mkdir -p $out/lib/sndio-6
    ln -s ${sndio}/lib/libsndio.so $out/lib/sndio-6/libsndio.so.6
  '';

  preFixup = ''
    mkdir -p $out/share/{applications,icons/hicolor/scalable/apps,mime/packages}
    pushd ${src}/distribution/linux

    install -D ./Ryujinx.desktop $out/share/applications/Ryujinx.desktop
    install -D ./Ryujinx.sh $out/bin/Ryujinx.sh
    install -D ./mime/Ryujinx.xml $out/share/mime/packages/Ryujinx.xml
    install -D ../misc/Logo.svg $out/share/icons/hicolor/scalable/apps/Ryujinx.svg

    substituteInPlace $out/share/applications/Ryujinx.desktop \
      --replace "Ryujinx.sh %f" "$out/bin/Ryujinx.sh %f"

    ln -s $out/bin/Ryujinx $out/bin/ryujinx

    popd
  '';

  meta = with lib; {
    homepage = "https://ryujinx.org/";
    changelog = "https://github.com/Ryujinx/Ryujinx/wiki/Changelog";
    description = "Experimental Nintendo Switch Emulator written in C#";
    longDescription = ''
      Ryujinx is an open-source Nintendo Switch emulator, created by gdkchan,
      written in C#. This emulator aims at providing excellent accuracy and
      performance, a user-friendly interface and consistent builds. It was
      written from scratch and development on the project began in September
      2017.
    '';
    license = licenses.mit;
    maintainers = with maintainers; [ starr-dusT jk artemist ];
    platforms = [ "x86_64-linux" "aarch64-linux" ];
    mainProgram = "Ryujinx";
  };
}
