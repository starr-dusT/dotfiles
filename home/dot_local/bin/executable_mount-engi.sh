#!/usr/bin/env bash
# mount-engi

function display_help() {
    echo "usage: $(basename "${0}")" 
    echo "Mount engi samba share to /mnt/engi"
}

while getopts ":hv" opt; do
    case ${opt} in
        h )
            display_help
            exit 0
            ;;
    esac
done

engi_path="/mnt/engi"

# Mount drive if it isn't
if mountpoint -q "${engi_path}"; then
    echo "engi already mounted"
    exit 0
else
    sudo mount -t cifs -o rw,uid=$(id -u $(whoami)),gid=$(id -g $(whoami)),vers=3.0,credentials=/home/tstarr/.smb //torus/private "${engi_path}" 2>/dev/null
fi

# Check drive mounted correctly
if mountpoint -q "${engi_path}"; then
  echo "engi successfully mounted"
else
  echo "engi failed to mount"
fi
