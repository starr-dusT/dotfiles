{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.emulation;
in {
  options.modules.gaming.emulation.enable = lib.mkEnableOption "emulation";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      ryujinx # Fuck Nintendo
      yuzu # Fuck Nintendo again
      sudachi # Nintendo Switch emulator written in C++
      dolphin-emu # GameCube and Wii emulator
      ppsspp # PSP emulator
      mgba # Game Boy Advance emulator
      rpcs3 # PS3 emulator/debugger
      (retroarch.override {
        cores = with libretro; [
          nestopia # Nintendo Entertainment System (NES) emulator
          snes9x # Super Nintendo Entertainment System (SNES) emulator
          mgba # Game Boy Advance emulator
          melonds # Nintendo DS emulator
          citra # Fuck Nintendo a third time
        ];
      })
    ];
  };
}
