{ pkgs, ... }:
let
  domain = "rssbridge.tstarr.us";
  dumpFolder = "/engi/backup/dumps/miniflux";
in 
{
  systemd.tmpfiles.rules = [
    "d ${dumpFolder} 0775 miniflux miniflux -"
  ];

  environment.systemPackages = [
    (pkgs.writeScriptBin "dump-miniflux" ''
      #!/bin/sh
      cd ${dumpFolder}
      [ -e miniflux-sql ] && rm miniflux-sql
      pg_dump miniflux > ${dumpFolder}/miniflux-sql
    '')
  ];

  services.postgresql = {
    enable = true;
    authentication = pkgs.lib.mkOverride 10 ''
      #type database  DBuser  auth-method
      local all       all     trust
    '';
  };

  services.miniflux = {
    enable = true;
    config = {
      PORT = "8087";
      # Break youtube embeds so they dont show
      YOUTUBE_EMBED_URL_OVERRIDE="https://";
    };
    # Set initial admin user/password
    adminCredentialsFile = pkgs.writeText "cred" ''
                             ADMIN_USERNAME=miniflux
                             ADMIN_PASSWORD=miniflux
                           '';
  };
  
  services.rss-bridge = {
    enable = true;
    config.system.enabled_bridges = [ "*" ];
    virtualHost = "${domain}";
  };
}
