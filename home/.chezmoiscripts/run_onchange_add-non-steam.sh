#!/usr/bin/env bash
# Add non-steam games to steam

apps=(
    "dolphin-emu Dolphin"
    "yuzu Yuzu"
)

for app in "${apps[@]}"
do
    set -- $app
    steamtinkerlaunch addnonsteamgame -ep="$1" -an="$2"
done
