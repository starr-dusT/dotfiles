{ ... }:
{
  flake.modules.nixos.pc =
    { pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        gamescope # Utility for running games using Valve's Steam Play compatibility layer with improved performance and compatibility
        mangohud # Vulkan and OpenGL overlay for monitoring FPS, temperatures, CPU/GPU load and more
        heroic # Native GOG Launcher
        jdk17 # Java Development Kit (JDK) version 17
        prismlauncher # Launcher for Prism, a web application framework
      ];

      programs.steam = {
        enable = true;
        package = with pkgs; steam.override { extraPkgs = pkgs: [ attr ]; }; # https://github.com/NixOS/nixpkgs/issues/236561
      };
    };
}
