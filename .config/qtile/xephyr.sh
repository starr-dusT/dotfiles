#!/usr/bin/env bash

function cleanup {
    echo "cleaning up!"
    xeph_qtile=0
}

xeph_qtile=1
Xephyr -br -ac -noreset -screen 800x600 :1 &
export DISPLAY=:1
qtile start &

trap cleanup EXIT
