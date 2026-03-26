#!/usr/bin/env bash

function display_help() {
    echo "usage: $(basename "$0") <volume>" 
    echo "Set volume of subwoofer with pySVS."
}

function read_volume() {
    vol=$(PGPASSWORD="$(cat /run/agenix/kube/homedb_pass)" psql -h homedb.vortex-1.lan \
                                                           -U tstarr -d homedb \
                                                           -p 30432 -t \
                                                           -c "SELECT volume from subwoofer_volume;")
    echo "${vol// /}"
}

while getopts ":hr" opt; do
    case $opt in
        h )
            display_help
            exit 0
            ;;
        r )
            read_volume
            exit 0
            ;;
    esac
done


args=(
    "54:B7:E5:57:1A:7B" # bluetooth mac for subwoofer
    --volume="$1"
)

if [ "$(hostname)" == "stormwalker" ]; then
  args+=(-b hci1)
fi

if [ "$1" == "A" ]; then
    vol=$(pySVS "${args[@]}" | sed "s/'/\"/g" | jq '.VOLUME')
    echo "Current subwoofer volume is $vol"
    notify-send "Current subwoofer volume is $vol"
    PGPASSWORD="$(cat /run/agenix/kube/homedb_pass)" psql -h homedb.vortex-1.lan \
                                                     -U tstarr -d homedb \
                                                     -p 30432 -t \
                                                     -c "UPDATE subwoofer_volume SET volume=$vol;"
else
    pySVS "${args[@]}" || {
        echo "Error setting subwoofer volume"
        notify-send "Error setting subwoofer volume"
        exit 1
    }

    echo "Set subwoofer volume to $1"
    notify-send "Set subwoofer volume to $1"
    # Bar reads this db to display current volume
    PGPASSWORD="$(cat /run/agenix/kube/homedb_pass)" psql -h homedb.vortex-1.lan \
                                                     -U tstarr -d homedb \
                                                     -p 30432 -t \
                                                     -c "UPDATE subwoofer_volume SET volume=$1;"
fi
