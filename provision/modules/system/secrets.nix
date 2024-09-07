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
      file = ../../secrets/git/github_personal.age;
      owner = "${user}";
      group = "users";
    };

    # ssh secrets
    age.secrets."ssh/kestrel/id_ed25519" = {
      file = ../../secrets/ssh/kestrel/id_ed25519.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."ssh/kestrel/id_ed25519.pub" = {
      file = ../../secrets/ssh/kestrel/id_ed25519.pub.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."ssh/torus/id_ed25519" = {
      file = ../../secrets/ssh/torus/id_ed25519.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."ssh/torus/id_ed25519.pub" = {
      file = ../../secrets/ssh/torus/id_ed25519.pub.age;
      owner = "${user}";
      group = "users";
    };

    # emu secrets
    age.secrets."emu/switch/prod.keys" = {
      file = ../../secrets/emu/switch/prod.keys.age;
      owner = "${user}";
      group = "users";
    };
    age.secrets."emu/switch/title.keys" = {
      file = ../../secrets/emu/switch/title.keys.age;
      owner = "${user}";
      group = "users";
    };
  };
}
