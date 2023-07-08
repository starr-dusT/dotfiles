#!/usr/bin/env bash

cons=$(nmcli -t -f NAME c show --active | grep $1)
if [ $cons ]; then
    nmcli c down $1 1> /dev/null
else
    nmcli c up $1 1> /dev/null
fi
