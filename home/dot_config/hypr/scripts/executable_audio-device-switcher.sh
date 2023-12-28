#!/usr/bin/env bash
#
if [[ $1 -eq 0 ]]; then
   notify-send "$(pactl list sinks | awk -F': ' '/State:.*RUNNING/{getline; getline; sub("^[ \t]+", "", $2); print "Active Sink: " $2}')"
else
    sink=$(pactl list sinks | grep "Sink #" | head -n $1 | tail -n 1)
    card=$(pactl list sinks | grep "alsa.card_name" | head -n $1 | tail -n 1)
    pactl set-default-sink $(echo $sink | cut -d "#" -f 2)
    notify-send "Active Sink: $(echo $card | cut -d "\"" -f 2)"
fi
