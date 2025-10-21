{ config, lib, pkgs, ... }:

let cfg = config.modules.optional.scripts;
    blk = config.modules.optional.scripts.blacklist;
    nblkd = ! (builtins.elem "mount-engi.sh" blk);
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.mkIf nblkd [
      (pkgs.writeShellScriptBin "mount-engi.sh" (builtins.readFile ./scripts/mount-engi.sh))
    ];
  };
}
