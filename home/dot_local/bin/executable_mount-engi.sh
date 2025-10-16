#!/usr/bin/env bash
# mount-engi

function display_help() {
    echo "usage: $(basename "${0}")" 
    echo "Mount engi samba share with gio"
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

# Mount drive if it isn't
if gio mount --list | grep engi > /dev/null 2>&1; then
    echo "engi already mounted"
    exit 0
else
    gio mount smb://torus/engi < /run/agenix/smb/torus &> /dev/null
fi

# Check drive mounted correctly
if gio mount --list | grep engi > /dev/null 2>&1; then
  echo "engi successfully mounted"
else
  echo "engi failed to mount"
fi
