#!/usr/bin/env bash
# Add non-steam games to steam

apps=(
    "dolphin-emu Dolphin"
    "yuzu Yuzu"
)

if command -v steamtinkerlaunch &> /dev/null
then
    for app in "${apps[@]}"
    do
        set -- $app
        steamtinkerlaunch addnonsteamgame -ep="$1" -an="$2"
    done
fi
