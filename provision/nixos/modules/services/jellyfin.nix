{ config, lib, pkgs, user, ... }:

let cfg = config.modules.services.jellyfin;
in {
  options.modules.services.jellyfin.enable = lib.mkEnableOption "jellyfin";
  config = lib.mkIf cfg.enable {
    services.jellyfin.enable = true;
    services.jellyfin.openFirewall = true;
    services.jellyfin.user = "${user}";
  };

}
