#!/usr/bin/env bash
# Add non-steam games to steam

steam_userid="47011563"
shortcuts_vdf="$HOME/.steam/root/userdata/${steam_userid}/config/shortcuts.vdf"
rm $shortcuts_vdf

mkdir -p "$HOME/.local/share/Steam/userdata/47011563/config/grid"

apps=(
    "/run/current-system/sw/bin/dolphin-emu Dolphin Emulator"
    "/run/current-system/sw/bin/yuzu Yuzu Emulator"
    "/run/current-system/sw/bin/ppsspp PPSSPP Emulator"
    "/run/current-system/sw/bin/mgba-qt mGBA Emulator"
)

if command -v steamtinkerlaunch &> /dev/null
then
    for app in "${apps[@]}"
    do
        set -- $app
        steamtinkerlaunch addnonsteamgame \
            -ep="$1" -an="$2" \
            --iconpath="$HOME/.local/share/chezmoi/img/$2/icons.ico" \
            --hero="$HOME/.local/share/chezmoi/img/$2/hero.png" \
            --logo="$HOME/.local/share/chezmoi/img/$2/logo.png" \
            --boxart="$HOME/.local/share/chezmoi/img/$2/boxart.png" \
            --tenfoot="$HOME/.local/share/chezmoi/img/$2/tenfoot.png" \
            --tags="$3" \
            --copy
    done
fi
