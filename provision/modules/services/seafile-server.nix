{ config, lib, pkgs, pkgs-unstable, user, ... }:

let 
    cfg = config.modules.services.seafile-server;
in {
  options.modules.services.seafile-server.enable = lib.mkEnableOption "seafile-server";
  config = lib.mkIf cfg.enable {
      services.seafile = {
        enable = true;
        adminEmail = "starrtyler88@gmail.com";
        initialAdminPassword = "dude";
        ccnetSettings.General.SERVICE_URL = "https://wiki.tstarr.us";
      };
  };
}
