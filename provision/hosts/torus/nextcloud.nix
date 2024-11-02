{ config, lib, pkgs, user, ... }:
let
  dumpFolder = "/engi/backup/dumps/nextcloud";
  domain = "cloud.tstarr.us";
in {
  environment.systemPackages = with pkgs; [
    cron
    zip
    rsync
    (pkgs.writeScriptBin "dump-nextcloud" ''
      #!/bin/sh
      cd ${dumpFolder}
      [ -e nextcloud-sql ] && rm nextcloud-sql 
      nextcloud-occ maintenance:mode --on
      mysqldump --single-transaction nextcloud > ${dumpFolder}/nextcloud-sql
      nextcloud-occ maintenance:mode --off
    '')
  ];

  systemd.tmpfiles.rules = [
    "d ${dumpFolder} 0775 nextcloud nextcloud -"
  ];
  
  # nextcloud secrets
  age.secrets."nextcloud/password" = {
    file = ../../secrets/nextcloud/password.age;
    owner = "nextcloud";
    group = "nextcloud";
  };

  services = {
    nginx.virtualHosts = {
      "${domain}" = {
        forceSSL = true;
        enableACME = true;
      };
    };

    nextcloud = {
      enable = true;
      hostName = "${domain}";

       # Need to manually increment with every major upgrade.
      package = pkgs.nextcloud29;

      # Let NixOS install and configure the database automatically.
      database.createLocally = true;

      # Let NixOS install and configure Redis caching automatically.
      configureRedis = true;

      # Increase the maximum file upload size to avoid problems uploading videos.
      maxUploadSize = "16G";
      https = true;
      autoUpdateApps.enable = true;
      settings = {
        overwriteprotocol = "https";
        default_phone_region = "US";
      };

      config = {
        dbtype = "mysql";
        adminuser = "admin";
        adminpassFile = "/run/agenix/nextcloud/password";
      };
    };
  };
}
