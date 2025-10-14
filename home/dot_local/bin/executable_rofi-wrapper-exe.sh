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

# Use rofi to select script in ${bin} and run redirecting stdout to notify-send
prog=$(ls -1 "${bin}" | grep -v "rofi" | my-rofi.sh "exe")
"${bin}/${prog}" | xargs -I {} notify-send "{}"
