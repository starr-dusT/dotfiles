#!/usr/bin/env bash
# Add non-steam games to steam

steam_userid="47011563"
shortcuts_vdf="$HOME/.steam/root/userdata/${steam_userid}/config/shortcuts.vdf"

apps=(
    " /run/current-system/sw/bin/dolphin-emu Dolphin"
    "/run/current-system/sw/bin/yuzu Yuzu"
    "/run/current-system/sw/bin/ppsspp PPSSPP"
)

if command -v steamtinkerlaunch &> /dev/null
then
    for app in "${apps[@]}"
    do
        set -- $app
        if ! cat $shortcuts_vdf | grep -q "$2"; then
            steamtinkerlaunch addnonsteamgame -ep="$1" -an="$2"
        fi
    done
fi
