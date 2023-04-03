#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: "${0}" <i3_mark> <launch_cmd>"
    echo "Example: ${0} 'scratch-emacs' 'emacsclient -c -a emacs'"
    exit 1
fi

I3_MARK=${1}
LAUNCH_CMD=${2}

scratchpad_show() {
    swaymsg "[con_mark=${I3_MARK}]" scratchpad show
}

# try showing the scratchpad window
if ! scratchpad_show; then
    # if there is no such window...

    # launch the application.
    eval "${LAUNCH_CMD}" &

    # Wait for the next window event.
    swaymsg -t subscribe  '[ "window" ]'

    # Set a mark
    swaymsg mark ${I3_MARK}

    # Move it to the scratchpad workspace
    swaymsg move scratchpad

    # show the scratchpad window
    scratchpad_show
fi
