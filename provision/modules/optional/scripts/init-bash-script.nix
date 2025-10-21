{ config, lib, pkgs, ... }:

let cfg = config.modules.optional.scripts;
    blk = config.modules.optional.scripts.blacklist;
    nblkd = ! (builtins.elem "init-bash-script.sh" blk);
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.mkIf nblkd [
      (pkgs.writeShellScriptBin "init-bash-script.sh" (builtins.readFile ./scripts/init-bash-script.sh))
    ];
  };
}
