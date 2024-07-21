{ config, lib, pkgs, user, inputs, ... }:

let cfg = config.modules.system.secrets;
in {
  options.modules.system.secrets.enable = lib.mkEnableOption "secrets";
  config = lib.mkIf cfg.enable {
     
    environment.systemPackages = [
      inputs.agenix.packages.x86_64-linux.default 
    ];
    
    age.secrets."git/github_personal" = {
      file = ../../age-secrets/git/github_personal.age;
      owner = "${user}";
      group = "users";
    };

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
