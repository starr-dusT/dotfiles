{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.steam;
in {
  options.modules.gaming.steam.enable = lib.mkEnableOption "steam";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      steamtinkerlaunch # Launcher and optimization tool for Steam games.
      gamescope # Utility for running games using Valve's Steam Play compatibility layer with improved performance and compatibility.
      mangohud # Vulkan and OpenGL overlay for monitoring FPS, temperatures, CPU/GPU load and more.
    ];

    hardware.graphics.enable = true; # this fixes the "glXChooseVisual failed" bug, context: https://github.com/NixOS/nixpkgs/issues/47932
    hardware.pulseaudio.support32Bit = config.hardware.pulseaudio.enable; # Enable 32bit pulseaudio support if pulseaudio is enabled
    hardware.steam-hardware.enable = true;

    programs.steam = {
       enable = true;
       package = with pkgs; steam.override { extraPkgs = pkgs: [ attr ]; }; # Workaround from: https://github.com/NixOS/nixpkgs/issues/236561
    };
  };
}
