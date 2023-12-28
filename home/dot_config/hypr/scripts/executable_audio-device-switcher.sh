#!/usr/bin/env bash
#
sink=$(pactl list sinks | grep "Sink #" | head -n $1 | tail -n 1)
pactl set-default-sink $(echo $sink | cut -d "#" -f 2)
