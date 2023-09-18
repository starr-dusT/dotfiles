{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.syncthing;
in {
  options.modules.services.syncthing.enable = lib.mkEnableOption "syncthing";
  config = lib.mkIf cfg.enable {

    environment.systemPackages = with pkgs; [ syncthing ];
    services.syncthing = {
      enable = true;
      user = "${user}";
      configDir = "/home/${user}/.config/syncthing";
    };
  };
}
