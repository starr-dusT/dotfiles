#!/usr/bin/env bash

function display_help() {
    echo "usage: $(basename "$0") <script-name>" 
    echo "Copy script template to current directory."
}

while getopts ":hv" opt; do
    case $opt in
        h )
            display_help
            exit 0
            ;;
    esac
done

cp ~/.local/share/chezmoi/resources/templates/bash.sh "./$1"
