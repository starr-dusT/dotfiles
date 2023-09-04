#!/usr/bin/env bash

apps=(
    "dolphin-emu Dolphin"
    "yuzu Yuzu"
)

for app in "${apps[@]}"
do
    set -- $app
    steamtinkerlaunch addnonsteamgame -ep="$1" -an="$2"
done
