{ config, lib, pkgs, user, ... }:
let
  domain = "rssbridge.tstarr.us";
in 
{
  imports = [
    ./rss-bridge.nix
  ];

  services.postgresql = {
    enable = true;
    authentication = pkgs.lib.mkOverride 10 ''
      #type database  DBuser  auth-method
      local all       all     trust
    '';
  };

  networking.firewall.allowedTCPPorts = [ 8087 ];
  networking.firewall.allowedUDPPorts = [ 8087 ];

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
  
  my-services.rss-bridge = {
    enable = true;
    whitelist = [ "*" ];
    virtualHost = "${domain}";
  };

  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
  };
}
