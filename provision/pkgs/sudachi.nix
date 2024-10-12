{
  lib,
  stdenv,
  fetchzip,
  fetchFromGitea,
  cmake,
  glslang,
  pkg-config,
  qttools,
  wrapQtAppsHook,
  boost,
  fmt,
  lz4,
  nlohmann_json,
  qtbase,
  qtmultimedia,
  qtwayland,
  qtwebengine,
  zlib,
  zstd,
  rapidjson,
  libva,
  libvdpau,
  nv-codec-headers-12,
  yasm,
  autoconf,
  automake,
  libtool,
  spirv-headers,
  catch2_3,
  vulkan-loader,
  vulkan-headers,
}:

let
  # Derived from externals/nx_tzdb/CMakeLists.txt
  nx_tzdb = fetchzip {
    url = "https://github.com/lat9nq/tzdb_to_nx/releases/download/221202/221202.zip";
    stripRoot = false;
    hash = "sha256-YOIElcKTiclem05trZsA3YJReozu/ex7jJAKD6nAMwc=";
  };
  suyu = fetchFromGitea {
    domain = "git.suyu.dev";
    owner = "suyu";
    repo = "suyu";
    rev = "ee365bad9501c73ff49936e72ec91cd9c3ce5c24";
    hash = "sha256-vw9VcSbCaG4MS0PL/fJ73CDALLbd3n0CBT7gkyp5hRc=";
    fetchSubmodules = true;
  };
in
stdenv.mkDerivation (finalAttrs: {
  pname = "sudachi";
  version = "1.0.11";
  
  src = fetchzip {
    url = "https://github.com/emuplace/sudachi.emuplace.app/releases/download/v1.0.11/latest.zip";
    hash = "sha256-aQ1cNFG6JCNrMz4N9uv6QRidjNwvyJqGunhwhJpvv4s=";
  };

  nativeBuildInputs = [
    cmake
    glslang
    pkg-config
    qttools
    wrapQtAppsHook
  ];

  buildInputs = [
    vulkan-headers
    boost
    fmt
    lz4
    nlohmann_json
    qtbase
    qtmultimedia
    qtwayland
    qtwebengine
    zlib
    zstd

    # vendored discord-rpc deps
    rapidjson

    # vendored ffmpeg deps
    libva # for accelerated video decode on non-nvidia
    libvdpau # for accelerated video decode on non-nvidia
    nv-codec-headers-12 # for accelerated video decode on nvidia
    yasm

    # vendored libusb deps
    autoconf
    automake
    libtool

    # vendored sirit deps
    spirv-headers
  ];

  cmakeFlags = [
    # actually has a noticeable performance impact
    "-DSUDACHI_ENABLE_LTO=ON"

    # build with qt6
    "-DENABLE_QT6=ON"

    # enable some optional features
    "-DSUDACHI_USE_QT_WEB_ENGINE=ON"
    "-DSUDACHI_USE_QT_MULTIMEDIA=ON"
    "-DUSE_DISCORD_PRESENCE=ON"

    # compatibility list currently 404's
    "-DENABLE_COMPATIBILITY_LIST_DOWNLOAD=OFF"

    # don't check for missing submodules
    "-DSUDACHI_CHECK_SUBMODULES=OFF"

    # use system libraries
    # NB: "external" here means "from the externals/ directory in the source",
    # so "off" means "use system"
    "-DSUDACHI_USE_EXTERNAL_VULKAN_HEADERS=OFF"

    # Disable SDL2 audio
    # Sudachi seems to have a bug that ignores SUDACHI_USE_EXTERNAL_SDL2 flag
    "-DENABLE_SDL2=OFF"

    # don't use system ffmpeg, sudachi uses internal APIs
    "-DSUDACHI_USE_BUNDLED_FFMPEG=ON"
    "-DFFmpeg_COMPONENTS='swscale;avutil;avfilter;avcodec'"
    "-DFFmpeg_PREFIX=$src/externals/ffmpeg/ffmpeg"

    # use system spriv headers for sirit
    "-DSIRIT_USE_SYSTEM_SPIRV_HEADERS=ON"
  ];

  # Does some handrolled SIMD
  env.NIX_CFLAGS_COMPILE = lib.optionalString stdenv.hostPlatform.isx86_64 "-msse4.1";

  # This changes `ir/opt` to `ir/var/empty` in `externals/dynarmic/src/dynarmic/CMakeLists.txt`
  # making the build fail, as that path does not exist
  dontFixCmake = true;

  preConfigure = ''
    # see https://github.com/NixOS/nixpkgs/issues/114044, setting this through cmakeFlags does not work.
    cmakeFlagsArray+=(
      "-DTITLE_BAR_FORMAT_IDLE=${finalAttrs.pname} | ${finalAttrs.version} (nixpkgs) {}"
      "-DTITLE_BAR_FORMAT_RUNNING=${finalAttrs.pname} | ${finalAttrs.version} (nixpkgs) | {}"
    )

    # Copy suyu externals
    rm -R externals/
    cp -R ${suyu}/externals .

    # replace "SUYU" with "SUDACHI" in externals cmake args
    chmod u+rw -R externals/
    grep -rl SUYU | xargs sed -i 's/SUYU/SUDACHI/g'

    # provide pre-downloaded tz data
    mkdir -p build/externals/nx_tzdb
    ln -s ${nx_tzdb} build/externals/nx_tzdb/nx_tzdb
  '';

  doCheck = true;
  checkInputs = [ catch2_3 ];

  # Fixes vulkan detection.
  # FIXME: patchelf --add-rpath corrupts the binary for some reason, investigate
  qtWrapperArgs = [ "--prefix LD_LIBRARY_PATH : ${vulkan-loader}/lib" ];

  postInstall = ''
    install -Dm444 "$src/dist/72-sudachi-input.rules" "$out/lib/udev/rules.d/72-sudachi-input.rules"
    mkdir -p "$out/share/sudachi"
    cp -R "$src" "$out/share/sudachi/src"
  '';

  meta = with lib; {
    description = "Nintendo Switch emulator written in C++";
    homepage = "https://sudachiemu.com/";
    changelog = "https://sudachi.emuplace.app/";
    mainProgram = "sudachi";
    platforms = [
      "aarch64-linux"
      "x86_64-linux"
    ];

    license = with licenses; [
      gpl3Plus

      # Icons
      asl20
      mit
      cc0
    ];

    maintainers = with maintainers; [ starr-dusT ];
  };
})
