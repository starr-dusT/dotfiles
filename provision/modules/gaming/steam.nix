{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.steam;
in {
  options.modules.gaming.steam.enable = lib.mkEnableOption "steam";
  config = lib.mkIf cfg.enable {
    hardware.opengl = { # this fixes the "glXChooseVisual failed" bug, context: https://github.com/NixOS/nixpkgs/issues/47932
      enable = true;
      driSupport32Bit = true;
    };

    # optionally enable 32bit pulseaudio support if pulseaudio is enabled
    hardware.pulseaudio.support32Bit = config.hardware.pulseaudio.enable;

    hardware.steam-hardware.enable = true;

    environment.systemPackages = with pkgs; [ 
      steam
      steamtinkerlaunch
      gamescope
      yuzu-early-access
      dolphin-emu
      ppsspp
      mgba
      ares
      sunshine
      moonlight-qt
      prismlauncher
      jdk17
    ];
  };

}
