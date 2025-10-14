#!/usr/bin/env bash
# sink-switch 

function display_help() {
    echo "usage: $(basename "${0}") <sink-partial-match>" 
    echo "Switch audio sink with parital string match. If arguement is not set"
    echo "rofi will be used to set it."
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

# Set sink to first arguement or use rofi to select
if [ "${1}" ]; then
    name="${1}"
else
    pre="Living Room|Desktop"
    sel=$(echo "${pre}" | my-rofi.sh -sep "|" -p "sink")
    # Convert selection to string for sink
    case ${sel} in
        "Living Room" )
            name="Dragon"
            ;;
        "Desktop" )
            name="Starship"
            ;;
        * )
            name="${sel}"
    esac
fi

# Find sink ID from sub string
sink=$(wpctl status | 
    awk '/Audio/{flag=1} /Video/{flag=0} flag' | 
    awk '/Sinks:/{flag=1; next} /Sources:/{flag=0} flag' | 
    grep -E "${name}" | 
    awk '{for(i=1;i<=NF;i++) if ($i ~ /^[0-9]+\.$/) { print int($i); exit }}'
)

# Set sink to ID
wpctl set-default "${sink}" &> /dev/null || {
    echo "Error setting sink"
    exit 1
}
