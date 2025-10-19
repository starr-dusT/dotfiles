#!/usr/bin/env bash
# subwoofer-volume 

function display_help() {
    echo "usage: $(basename "${0}") <volume>" 
    echo "Set volume of subwoofer with pySVS."
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

args=(
    "54:B7:E5:57:1A:7B" # bluetooth mac for subwoofer
    --volume="${1}"
)

if [ "$(hostname)" == "stormwalker" ]; then
  args+=(-b hci1)
fi
  
pySVS "${args[@]}" 2>/dev/null || {
    echo "Error setting subwoofer volume"
    exit 1
}
echo "Set subwoofer volume to ${vol}"
echo "${vol}" > /tmp/svs # bar reads this file to display current volume
