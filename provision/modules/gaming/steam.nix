{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.steam;
in {
  options.modules.gaming.steam.enable = lib.mkEnableOption "steam";
  config = lib.mkIf cfg.enable {
    hardware.graphics.enable = true; # this fixes the "glXChooseVisual failed" bug, context: https://github.com/NixOS/nixpkgs/issues/47932

    # optionally enable 32bit pulseaudio support if pulseaudio is enabled
    hardware.pulseaudio.support32Bit = config.hardware.pulseaudio.enable;

    hardware.steam-hardware.enable = true;
    # Digital distribution platform for purchasing and playing video games.
    programs.steam = {
       enable = true;
       # Workaround from: https://github.com/NixOS/nixpkgs/issues/236561
       package = with pkgs; steam.override { extraPkgs = pkgs: [ attr ]; };
    };
    environment.systemPackages = with pkgs; [ 
      steamtinkerlaunch # Launcher and optimization tool for Steam games.
      gamescope # Utility for running games using Valve's Steam Play compatibility layer with improved performance and compatibility.
      mangohud
    ];
  };
}
