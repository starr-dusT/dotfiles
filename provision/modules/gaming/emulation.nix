{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.emulation;
in {
  options.modules.gaming.emulation.enable = lib.mkEnableOption "emulation";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      ryujinx # Fuck Nintendo!
      sudachi # Nintendo Switch emulator written in C++.
      yuzu # Fuck Nintendo!
      dolphin-emu # GameCube and Wii emulator.
      ppsspp # PSP emulator.
      mgba # Game Boy Advance emulator.
      (retroarch.override {
        cores = with libretro; [
          nestopia # Nintendo Entertainment System (NES) emulator.
          snes9x # Super Nintendo Entertainment System (SNES) emulator.
          mgba # Game Boy Advance emulator.
          melonds # Nintendo DS emulator.
          citra # Fuck Nintendo!
        ];
      })
    ];

    # Provide dumped keys for switch emulation
    age.secrets."emu/switch/prod.keys" = {
      file = ../../secrets/emu/switch/prod.keys.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."emu/switch/title.keys" = {
      file = ../../secrets/emu/switch/title.keys.age;
      owner = "${user}";
      group = "users";
    };
  };
}
