#!/usr/bin/env bash
# rofi-wrapper-exe

function display_help() {
    echo "usage: $(basename "${0}")" 
    echo "Use rofi to select and run scripts in ~/.local/bin"
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

bin="$HOME/.local/bin"
exclude="rofi|init-bash-script.sh"

# Use rofi to select script in ${bin} and run redirecting stdout to notify-send
prog=$(ls -1 "${bin}" | grep -Ev "${exclude}" | my-rofi.sh -p "exe")
"${bin}/${prog}" | xargs -I {} notify-send "{}"
