#!/usr/bin/env bash
# display-switch

function display_help() {
    echo "usage: $(basename "${0}") <dispay selection>" 
    echo "Switch display between Desktop and Living room for Kestrel."
    echo "Only valid selections are \"Living Room\" and \"Desktop\""
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

# Set display to first arguement or use rofi to select
if [ "${1}" ]; then
    sel="${1}"
else
    pre="Living Room|Desktop"
    sel=$(echo "${pre}" | my-rofi.sh -sep "|" -p "sink")
fi

# Get location of mutter in nix store to work around gdctl not being in PATH
# https://github.com/NixOS/nixpkgs/issues/416824
mutter=$(echo "$(nix eval nixpkgs#mutter.outPath)" | sed -r 's/\"//g')
case ${sel} in
    "Living Room" )
        ${mutter}/bin/gdctl set --logical-monitor --primary --monitor HDMI-1 --mode 2560x1440@59.951 --scale 1.5
        ;;
    "Desktop" )
        ${mutter}/bin/gdctl set --logical-monitor --primary --monitor DP-2 --mode 2560x1440@143.912 --scale 1 --logical-monitor --monitor DP-1 --mode 2560x1440@143.973 --scale 1 --left-of DP-2;
        ;;
    * )
        echo "Only valid selections are \"Living Room\" and \"Desktop\""
esac
