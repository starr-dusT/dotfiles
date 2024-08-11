{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.emulation;
in {
  options.modules.gaming.emulation.enable = lib.mkEnableOption "emulation";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      yuzu-archive # Nintendo Switch emulator.
      ryujinx # Nintendo Switch emulator written in C#.
      dolphin-emu # GameCube and Wii emulator.
      ppsspp # PSP emulator.
      mgba # Game Boy Advance emulator.
      (retroarch.override {
        cores = with libretro; [
          nestopia # Nintendo Entertainment System (NES) emulator.
          snes9x # Super Nintendo Entertainment System (SNES) emulator.
          mgba # Game Boy Advance emulator.
          melonds # Nintendo DS emulator.
          citra # Nintendo 3DS emulator.
        ];
      })
    ];
  };
}
