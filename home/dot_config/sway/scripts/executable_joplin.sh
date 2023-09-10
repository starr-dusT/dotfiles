#!/usr/bin/env bash

while ! [[ $(swaymsg -t get_tree | grep '"class": "Joplin"') ]]
do
    sleep .05
done
swaymsg '[class="Joplin"]' move container to workspace "7:note"
