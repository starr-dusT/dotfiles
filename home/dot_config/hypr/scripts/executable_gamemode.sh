#!/usr/bin/env bash

status=$(gamemoded -s)
if [ "$status" == "gamemode is inactive" ]; then
    gamemoded -r &
    killall swayidle
else
    killall gamemoded
    ~/.config/hypr/scripts/idle.sh
fi
