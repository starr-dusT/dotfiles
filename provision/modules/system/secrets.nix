{ config, lib, pkgs, user, inputs, ... }:

let cfg = config.modules.system.secrets;
in {
  options.modules.system.secrets.enable = lib.mkEnableOption "secrets";
  config = lib.mkIf cfg.enable {
     
    environment.systemPackages = [
      inputs.agenix.packages.x86_64-linux.default 
    ];
   
    # git secrets
    age.secrets."git/github_personal" = {
      file = ../../age-secrets/git/github_personal.age;
      owner = "${user}";
      group = "users";
    };

    # wireguard secrets
    age.secrets."wireguard/kestrel".file = ../../age-secrets/wireguard/kestrel.age;
    #age.secrets."wireguard/bulwark".file = ../../age-secrets/wireguard/bulwark.age;
    #age.secrets."wireguard/adjudicator".file = ../../age-secrets/wireguard/adjudicator.age;
    #age.secrets."wireguard/torus".file = ../../age-secrets/wireguard/torus.age;
  };
}
