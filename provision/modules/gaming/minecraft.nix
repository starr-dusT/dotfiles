{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.minecraft;
in {
  options.modules.gaming.minecraft.enable = lib.mkEnableOption "minecraft";
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ 
      prismlauncher # Launcher for Prism, a web application framework.
      jdk17 # Java Development Kit (JDK) version 17.
    ];
  };
}
