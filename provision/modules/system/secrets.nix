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
  };
}
