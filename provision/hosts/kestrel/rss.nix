{ config, lib, pkgs, user, ... }:
{

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
      LISTEN_ADDR = "localhost:8087";
      # Break youtube embeds so they dont show
      YOUTUBE_EMBED_URL_OVERRIDE="https://";
    };
    adminCredentialsFile = pkgs.writeText "cred" ''
                             ADMIN_USERNAME=miniflux
                             ADMIN_PASSWORD=miniflux
                           '';
  };
}
