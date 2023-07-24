{ config, lib, pkgs, user, ... }:

let cfg = config.modules.server.jellyfin;
in {
  options.modules.server.jellyfin.enable = lib.mkEnableOption "jellyfin";
  config = lib.mkIf cfg.enable {
    services.jellyfin.enable = true;
  };

}
