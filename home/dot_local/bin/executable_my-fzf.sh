#!/usr/bin/env bash
# my-fzf

function display_help() {
    echo "usage: $(basename "${0}")" 
    echo "Run fzf with my custom command line options"
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

#cmd="echo \""${1}"\" | fzf --print-query > /tmp/fzf"
#kitty -e sh -c "${cmd}" &> /dev/null # Open kitty and make selection
#tail -n +2 /tmp/fzf # Return selection removing top line of file
fzf --print-query | tail -1
