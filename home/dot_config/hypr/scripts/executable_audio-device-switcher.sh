#!/usr/bin/env bash

cards=("HD-Audio Generic" "AudioQuest DragonFly Red v1.0")
sink=$(pactl list sinks | grep -E "Sink #|alsa.card_name" | grep -B 1 "${cards[$1-1]}" | grep -v "${cards[$1-1]}")
pactl set-default-sink $(echo $sink | cut -d "#" -f 2)
notify-send "Active Sink: ${cards[$1-1]}"
