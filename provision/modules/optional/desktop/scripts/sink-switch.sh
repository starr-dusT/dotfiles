#!/usr/bin/env bash

function display_help() {
    echo "Usage: $(basename "$0") <sink-partial-match>" 
    echo "Switch audio sink with parital string match."
}

while getopts ":hv" opt; do
    case $opt in
        h )
            display_help
            exit 0
            ;;
    esac
done

# Find sink ID from sub string
sink=$(wpctl status | 
    awk '/Audio/{flag=1} /Video/{flag=0} flag' | 
    awk '/Sinks:/{flag=1; next} /Sources:/{flag=0} flag' | 
    grep -E "$1" | 
    awk '{for(i=1;i<=NF;i++) if ($i ~ /^[0-9]+\.$/) { print int($i); exit }}'
)

# Set sink to ID
wpctl set-default "$sink" &> /dev/null || {
    echo "Error setting sink"
    notify-send "Error setting sink"
    exit 1
}
