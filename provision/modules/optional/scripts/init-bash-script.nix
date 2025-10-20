{ config, lib, pkgs, user, ... }:

let cfg = config.modules.scripts;
    blk = config.modules.scripts.blacklist;
    nblkd = ! (builtins.elem "init-bash-script.nix" blk);
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.mkIf nblkd [
      (pkgs.writeShellScriptBin "init-bash-script.sh" ''
        function display_help() {
            echo "usage: $(basename "$0") <script-name>" 
            echo "Copy script template to current directory."
        }
        
        while getopts ":hv" opt; do
            case $opt in
                h )
                    display_help
                    exit 0
                    ;;
            esac
        done
        
        cp ~/.local/share/chezmoi/resources/templates/bash.sh "./$1"
      '')
    ];
  };
}
