{ config, lib, user, ... }:

let cfg = config.modules.optional.programs.docker;
in {
  options.modules.optional.programs.docker = with lib; {
    enable = lib.mkOption {
      type = types.bool;
      default = false;
    };
    storageDriver = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker.enable = true;
    virtualisation.docker.storageDriver = lib.mkIf (cfg.storageDriver != null) "${cfg.storageDriver}";

    users.users.${user} = {
      extraGroups = [ "docker" ];
    };
  };
}
