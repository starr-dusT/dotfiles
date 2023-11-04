{ config, lib, pkgs, user, ... }:

let cfg = config.modules.system.backup;
in {
  options.modules.system.backup.enable = lib.mkEnableOption "backup";
  config = lib.mkIf cfg.enable {
  };

}
