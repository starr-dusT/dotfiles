{ config, lib, pkgs, user, ... }:
{
  networking.firewall.allowedTCPPorts = [ 5232 ];
  networking.firewall.allowedUDPPorts = [ 5232 ];
  
  services.radicale = {
    enable = true;
    settings = {
      server = {
        hosts = ["0.0.0.0:5232" "[::]:5232"];
      };
      auth = {
        type = "htpasswd";
        htpasswd_filename = "/run/secrets/radicale/users";
        htpasswd_encryption = "plain";
      };
      storage = {
        filesystem_folder = "/var/lib/radicale/collections";
      };
    };
  };
}
