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

sleep 2

case $val in
    "kestrel-living" )
        /run/current-system/sw/bin/gdctl set "${args[@]}" --logical-monitor --primary --monitor HDMI-1 --mode 2560x1440@59.951 --scale 1.33
        ;;
    "kestrel-desktop" )
        /run/current-system/sw/bin/gdctl set "${args[@]}" --logical-monitor --primary --monitor DP-1 --mode 2560x1440@143.912 --scale 1 --logical-monitor --monitor DP-2 --mode 2560x1440@143.973 --scale 1 --left-of DP-1
        ;;
    * )
        echo "Only valid selections are kestrel-living, kestrel-desktop"
        notify-send "Only valid selections are kestrel-living, kestrel-desktop"
esac
