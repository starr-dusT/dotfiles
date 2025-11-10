#!/usr/bin/env bash

function display_help() {
    echo "usage: $(basename "$0") <comma seperated hostnames>" 
    echo "ssh and update remote hosts."
}

while getopts ":hv" opt; do
    case $opt in
        h )
            display_help
            exit 0
            ;;
    esac
done

IFS=','
read -ra host_arr <<< "$1"
echo "$hostnames"
for item in "${host_arr[@]}"; do 
    ssh "tstarr@$item" -t "bash -c 'cd ~/.local/share/chezmoi && git pull origin master && just'";
done
