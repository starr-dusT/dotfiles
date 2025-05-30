{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.pc;
in {
  options.modules.gaming.pc.enable = lib.mkEnableOption "pc";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      gamescope # Utility for running games using Valve's Steam Play compatibility layer with improved performance and compatibility
      mangohud # Vulkan and OpenGL overlay for monitoring FPS, temperatures, CPU/GPU load and more
      steamtinkerlaunch # Launcher and optimization tool for Steam games
      heroic # Native GOG Launcher
    ];

    hardware.graphics.enable = true; # https://github.com/NixOS/nixpkgs/issues/47932
    services.pulseaudio.support32Bit = config.hardware.pulseaudio.enable;
    hardware.steam-hardware.enable = true;

    programs.steam = {
       enable = true;
       package = with pkgs; steam.override { extraPkgs = pkgs: [ attr ]; }; # https://github.com/NixOS/nixpkgs/issues/236561
    };
  };
}
