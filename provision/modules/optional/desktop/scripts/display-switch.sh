#!/usr/bin/env bash

function display_help() {
    echo "usage: $(basename "$0") <dispay selection>" 
    echo "Change display setup with gdctl."
    echo "Only valid selections are: kestrel-living, kestrel-desktop"
}

args=()
val="$1"

while getopts "p:h" opt; do
    case $opt in
        p )
            args=(--persistent)
            val="$2"
            ;;
        h )
            display_help
            exit 0
            ;;
    esac
done

# Get location of mutter in nix store to work around gdctl not being in PATH
# https://github.com/NixOS/nixpkgs/issues/416824
mutter=$(echo "$(nix eval nixpkgs#mutter.outPath)" | sed -r 's/\"//g')
case $val in
    "kestrel-living" )
        "$mutter/bin/gdctl" set "${args[@]}" --logical-monitor --primary --monitor HDMI-1 --mode 2560x1440@59.951 --scale 1.5
        ;;
    "kestrel-desktop" )
        "$mutter/bin/gdctl" set "${args[@]}" --logical-monitor --primary --monitor DP-2 --mode 2560x1440@143.912 --scale 1 --logical-monitor --monitor DP-1 --mode 2560x1440@143.973 --scale 1 --left-of DP-2
        ;;
    * )
        echo "Only valid selections are kestrel-living, kestrel-desktop"
        notify-send "Only valid selections are kestrel-living, kestrel-desktop"
esac
