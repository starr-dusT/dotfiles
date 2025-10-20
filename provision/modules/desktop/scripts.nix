{ config, lib, pkgs, user, ... }:

let cfg = config.modules.desktop;
in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "sink-switch.sh" ''
        function display_help() {
            echo "Usage: $(basename "$0") <sink-partial-match>" 
            echo "Switch audio sink with parital string match."
        }
        
        while getopts ":hv" opt; do
            case $opt in
                h )
                    display_help
                    exit 0
                    ;;
            esac
        done

        # Find sink ID from sub string
        sink=$(wpctl status | 
            awk '/Audio/{flag=1} /Video/{flag=0} flag' | 
            awk '/Sinks:/{flag=1; next} /Sources:/{flag=0} flag' | 
            grep -E "$1" | 
            awk '{for(i=1;i<=NF;i++) if ($i ~ /^[0-9]+\.$/) { print int($i); exit }}'
        )
        
        # Set sink to ID
        wpctl set-default "$sink" &> /dev/null || {
            echo "Error setting sink"
            notify-send "Error setting sink"
            exit 1
        }
      '')

      (pkgs.writeShellScriptBin "display-switch.sh" ''
        function display_help() {
            echo "usage: $(basename "$0") <dispay selection>" 
            echo "Change display setup with gdctl."
            echo "Only valid selections are: kestrel-living, kestrel-desktop"
        }
        
        while getopts ":hv" opt; do
            case $opt in
                h )
                    display_help
                    exit 0
                    ;;
            esac
        done
        
        # Get location of mutter in nix store to work around gdctl not being in PATH
        # https://github.com/NixOS/nixpkgs/issues/416824
        mutter=$(echo "$(nix eval nixpkgs#mutter.outPath)" | sed -r 's/\"//g')
        case $1 in
            "kestrel-living" )
                "$mutter/bin/gdctl" set --persistent --logical-monitor --primary --monitor HDMI-1 --mode 2560x1440@59.951 --scale 1.5
                ;;
            "kestrel-desktop" )
                "$mutter/bin/gdctl" set --persistent --logical-monitor --primary --monitor DP-2 --mode 2560x1440@143.912 --scale 1 --logical-monitor --monitor DP-1 --mode 2560x1440@143.973 --scale 1 --left-of DP-2;
                ;;
            * )
                echo "Only valid selections are kestrel-living, kestrel-desktop"
                notify-send "Only valid selections are kestrel-living, kestrel-desktop"
        esac
      '')

      (pkgs.writeShellScriptBin "subwoofer-volume.sh" ''
        function display_help() {
            echo "usage: $(basename "$0") <volume>" 
            echo "Set volume of subwoofer with pySVS."
        }
        
        while getopts ":hv" opt; do
            case $opt in
                h )
                    display_help
                    exit 0
                    ;;
            esac
        done
        
        args=(
            "54:B7:E5:57:1A:7B" # bluetooth mac for subwoofer
            --volume="$1"
        )
        
        if [ "$(hostname)" == "stormwalker" ]; then
          args+=(-b hci1)
        fi
          
        pySVS "''${args[@]}" 2>/dev/null || {
            echo "Error setting subwoofer volume"
            notify-send "Error setting subwoofer volume"
            exit 1
        }
        echo "Set subwoofer volume to $1"
        notify-send "Set subwoofer volume to $1"
        echo "$vol" > /tmp/svs # bar reads this file to display current volume
      '')
    ];
  };
}
