#!/usr/bin/env bash
set -euo pipefail

wal -i ~/media/pictures/wallpapers/random_wallpapers/sierra_red_mountains.jpg
lxsession &
xsetroot -cursor_name left_ptr
imwheel -b 45
