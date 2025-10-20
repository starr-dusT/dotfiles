{ config, lib, pkgs, user, ... }:

let cfg = config.modules.scripts;
    blk = config.modules.scripts.blacklist;
    nblkd = ! (builtins.elem "mount-engi.nix" blk);
in {
  config = lib.mkIf cfg.enable {
    environment.shellAliases = lib.mkIf nblkd {
      me = "mount-engi.sh";
    };

    environment.systemPackages = lib.mkIf nblkd [
      (pkgs.writeShellScriptBin "mount-engi.sh" ''
        function display_help() {
            echo "usage: $(basename "$0")" 
            echo "Mount engi samba share with gio"
        }
        
        while getopts ":hv" opt; do
            case $opt in
                h )
                    display_help
                    exit 0
                    ;;
            esac
        done
        
        # Mount drive if it isn't
        if gio mount --list | grep engi > /dev/null 2>&1; then
            echo "engi already mounted"
            notify-send "engi already mounted"
            exit 0
        else
            gio mount smb://torus/engi < /run/agenix/smb/torus &> /dev/null
        fi
        
        # Check drive mounted correctly
        if gio mount --list | grep engi > /dev/null 2>&1; then
          echo "engi successfully mounted"
          notify-send "engi successfully mounted"
        else
          echo "engi failed to mount"
          notify-send "engi failed to mount"
        fi
      '')
    ];
  };
}
