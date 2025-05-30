{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.emulation;
    retroarchWithCores = (pkgs.retroarch.withCores (cores: with cores; [
      nestopia # Nintendo Entertainment System (NES) emulator
      snes9x # Super Nintendo Entertainment System (SNES) emulator
      mgba # Game Boy Advance emulator
      melonds # Nintendo DS emulator
      citra # Fuck Nintendo a third time
    ]));
in {
  options.modules.gaming.emulation.enable = lib.mkEnableOption "emulation";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      ryubing
      dolphin-emu # GameCube and Wii emulator
      ppsspp # PSP emulator
      mgba # Game Boy Advance emulator
      rpcs3 # PS3 emulator/debugger
      retroarchWithCores # frontend for emulators and specific cores
      openmw # Open-source open-world RPG game engine that supports playing Morrowind
    ];
  };
}
