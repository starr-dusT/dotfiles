#!/usr/bin/env bash

status=$(gamemoded -s)
if [ "$status" == "gamemode is inactive" ]; then
    gamemoded -r &
else
    killall gamemoded
fi
