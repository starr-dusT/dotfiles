{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.misc;
in {
  options.modules.gaming.misc.enable = lib.mkEnableOption "misc";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      sunshine # Utility for streaming Android device display to a computer.
      moonlight-qt # Qt-based client for NVIDIA GameStream, allowing streaming of PC games to other devices.
      prismlauncher # Launcher for Prism, a web application framework.
      jdk17 # Java Development Kit (JDK) version 17.
    ];
  };
}
