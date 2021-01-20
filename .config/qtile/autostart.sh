#!/usr/bin/env bash
set -euo pipefail

nitrogen --restore &
lxsession &
xsetroot -cursor_name left_ptr
imwheel -b 45
