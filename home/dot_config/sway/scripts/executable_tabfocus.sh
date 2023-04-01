#!/usr/bin/env python

import i3ipc
import sys

move_normal =  {"h": "focus left",
                "j": "focus down",
                "k": "focus up",
                "l": "focus right"}

move_tabbed =  {"h": "focus parent; focus left",
                "j": "focus right",
                "k": "focus left",
                "l": "focus parent; focus right"}

i3 = i3ipc.Connection()

# Get the focused container
focused = i3.get_tree().find_focused()

# Get the layout of the parent container
layout = focused.parent.layout

if layout == "tabbed":
    i3.command(move_tabbed[sys.argv[1]])
else:
    i3.command(move_normal[sys.argv[1]])
