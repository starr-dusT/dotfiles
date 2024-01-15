{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.secrets;
in {
  options.modules.system.secrets.enable = lib.mkEnableOption "secrets";
  config = lib.mkIf cfg.enable {
    
    sops = {
      defaultSopsFile = ../../secrets/secrets.yaml;
      defaultSopsFormat = "yaml";
      age.keyFile = "/home/${user}/.config/sops/age/keys.txt";

      # Keys
      secrets."keys/github_personal" = { owner = "${user}"; };

      # Wireguard secrets
      secrets."wireguard/kestrel" = { owner = "${user}"; };
      secrets."wireguard/bulwark" = { owner = "${user}"; };
      secrets."wireguard/adjudicator" = { owner = "${user}"; };
      secrets."wireguard/torus" = { owner = "${user}"; };
    };
  };
}
