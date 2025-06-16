{ config, lib, pkgs, user, ... }:

let cfg = config.modules.programs.docker;
in {
  options.modules.programs.docker = with lib; {
    enable = lib.mkOption {
      type = types.bool;
      default = false;
    };
    keyPath = mkOption {
      type = types.path;
      default = ./key.pem;
    };
    storageDriver = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker.enable = true;
    virtualisation.docker.storageDriver = lib.mkIf (cfg.storageDriver != null) "${cfg.storageDriver}";
  };
}
