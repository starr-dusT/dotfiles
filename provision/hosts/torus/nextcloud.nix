{ config, lib, pkgs, user, ... }:
{
  environment.systemPackages = with pkgs; [
    cron
  ];
  
  # nextcloud secrets
  age.secrets."nextcloud/password" = {
    file = ../../age-secrets/nextcloud/password.age;
    owner = "nextcloud";
    group = "nextcloud";
  };

  services = {
    nginx.virtualHosts = {
      "cloud.tstarr.us" = {
        forceSSL = true;
        enableACME = true;
      };
    };

    nextcloud = {
      enable = true;
      hostName = "cloud.tstarr.us";

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
