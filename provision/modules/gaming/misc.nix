{ config, lib, pkgs, user, ... }:

let cfg = config.modules.gaming.misc;
in {
  options.modules.gaming.misc.enable = lib.mkEnableOption "misc";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ 
      sunshine
      moonlight-qt
      prismlauncher
      jdk17
    ];
  };
}
