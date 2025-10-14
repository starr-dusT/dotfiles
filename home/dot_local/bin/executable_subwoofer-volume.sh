#!/usr/bin/env bash
# subwoofer-volume 

function display_help() {
    echo "usage: $(basename "${0}") <volume>" 
    echo "Set volume of subwoofer with pySVS. If volume is not set rofi will"
    echo "allow selection."
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

# Set volume to first arguement or use rofi to select
if [ "${1}" ]; then
    vol="${1}"
else
    pre="-20|-12|-9|-3"
    vol=$(echo "${pre}" | my-rofi.sh -sep "|" -p "dB")
fi

args=(
    "54:B7:E5:57:1A:7B" # bluetooth mac for subwoofer
    --volume="${vol}"
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
