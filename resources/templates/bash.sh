#!/usr/bin/env bash
# <script name> 

function display_help() {
    echo "usage: $(basename "${0}") [<args>]" 
    echo "This script does something..."
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

echo "Script is doing something..."
