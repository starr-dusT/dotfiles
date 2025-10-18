#!/usr/bin/env bash
# my-rofi

function display_help() {
    echo "usage: $(basename "${0}") <prompt-title>" 
    echo "Run rofi with my custom command line options"
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

rofi -x11 -dmenu -theme gruvbox-dark -font "AdwaitaSans 20" "${@}"
