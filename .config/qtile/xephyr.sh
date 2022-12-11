#!/usr/bin/env bash

function cleanup {
    echo "cleaning up!"
    export xeph_qtile=0
}

export xeph_qtile=1
Xephyr -br -ac -noreset -screen 800x600 :1 &
export DISPLAY=:1
stumpwm &

trap cleanup EXIT
