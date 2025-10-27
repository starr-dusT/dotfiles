{ config, lib, pkgs, ... }:
let
  cfg = config.modules.optional.scripts;
  blk = config.modules.optional.scripts.blacklist;
  init-bash-script = !(builtins.elem "init-bash-script.sh" blk);
  mount-engi = !(builtins.elem "mount-engi.sh" blk);
in
{
  options.modules.optional.scripts = with lib; {
    enable = mkEnableOption "scripts";
    blacklist = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = ''
        list of scripts to blacklist for host.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.mkIf init-bash-script [
      (pkgs.writeShellScriptBin "init-bash-script.sh" (builtins.readFile ./scripts/init-bash-script.sh))
    ] ++ lib.mkIf mount-engi [ 
      (pkgs.writeShellScriptBin "mount-engi.sh" (builtins.readFile ./scripts/mount-engi.sh))
    ];
  };
}
