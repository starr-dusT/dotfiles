{ config, lib, pkgs, user, ... }:
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    ensureDatabases = ["tt_rss"];
    ensureUsers = [
      {
        name = "tt_rss";
        ensureDBOwnership = true;
      }
    ];
    # type, database, user, [address], auth-method, [auth-options]
    #authentication = ''
    #  local all all trust
    #'';
  };
  services.postgresqlBackup.enable = true;

  systemd.services.tt-rss = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

  services.tt-rss = {
    enable = true;
    virtualHost = "rss1.tstarr.us";
    selfUrlPath = "https://rss1.tstarr.us";
    database = {
      type = "pgsql";
      createLocally = false;
      name = "tt_rss";
      user = "tt_rss";
      password = null;
    };
    singleUserMode = true;
  };

  services.nginx = {
    virtualHosts."rss1.tstarr.us" = {
      enableACME = true;
      forceSSL = true;
    };
  };
}
