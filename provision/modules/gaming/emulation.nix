{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.emulation;
in {
  options.modules.gaming.emulation.enable = lib.mkEnableOption "emulation";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      yuzu-archive
      citra-archive
      dolphin-emu
      ppsspp
      mgba
      (retroarch.override {
        cores = with libretro; [
          nestopia
          snes9x
          mgba
          melonds
        ];
      })
    ];
  };
}
