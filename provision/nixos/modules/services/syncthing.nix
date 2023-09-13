{ config, lib, pkgs, ... }:

let cfg = config.modules.services.syncthing;
in {
  options.modules.services.syncthing.enable = lib.mkEnableOption "syncthing";
  config = lib.mkIf cfg.enable {
    # Install packages
    environment.systemPackages = with pkgs; [ syncthing ];
    services.syncthing = {
      enable = true;
      user = "${user}";
      configDir = "/home/${user}/.config/syncthing";
    };
  };
}
