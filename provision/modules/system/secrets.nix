{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.secrets;
in {
  options.modules.system.secrets.enable = lib.mkEnableOption "secrets";
  config = lib.mkIf cfg.enable {
    
    
    sops = let
      ncHost = (if config.networking.hostName == "torus" then "nextcloud" else "${user}");
    in {
      defaultSopsFile = ../../secrets/secrets.yaml;
      defaultSopsFormat = "yaml";
      age.keyFile = "/home/${user}/.config/sops/age/keys.txt";

      # Keys
      secrets."keys/github_personal" = { owner = "${user}"; };

      # Nextcloud password
      secrets."nextcloud/password" = { owner = "${ncHost}"; };

      # Wireguard secrets
      secrets."wireguard/kestrel" = { owner = "${user}"; };
      secrets."wireguard/bulwark" = { owner = "${user}"; };
      secrets."wireguard/adjudicator" = { owner = "${user}"; };
      secrets."wireguard/torus" = { owner = "${user}"; };
    };
  };
}
