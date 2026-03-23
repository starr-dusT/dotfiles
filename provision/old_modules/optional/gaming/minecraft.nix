{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.optional.gaming.minecraft;
in
{
  options.modules.optional.gaming.minecraft.enable = lib.mkEnableOption "minecraft";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      jdk17 # Java Development Kit (JDK) version 17
      prismlauncher # Launcher for Prism, a web application framework
    ];
  };
}
