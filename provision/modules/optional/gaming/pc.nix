{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.gaming.pc;
in
{
  options.modules.optional.gaming.pc.enable = lib.mkEnableOption "pc";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gamescope # Utility for running games using Valve's Steam Play compatibility layer with improved performance and compatibility
      mangohud # Vulkan and OpenGL overlay for monitoring FPS, temperatures, CPU/GPU load and more
      heroic # Native GOG Launcher
    ];

    programs.steam = {
      enable = true;
      package = with pkgs; steam.override { extraPkgs = pkgs: [ attr ]; }; # https://github.com/NixOS/nixpkgs/issues/236561
    };
  };
}
