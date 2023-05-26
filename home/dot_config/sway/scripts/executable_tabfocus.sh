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

move_tabbed_single =  {"h": "focus left",
                       "j": "focus right",
                       "k": "focus left",
                       "l": "focus right"}

i3 = i3ipc.Connection()

# Get the focused container
focused = i3.get_tree().find_focused()

# Get number of splits (v or h)
focused_workspace = focused.workspace()
num_splits = sum(1 for con in focused_workspace.descendants() if con.layout == 'splitv' or con.layout == 'splith')

# Get the layout of the parent container
layout = focused.parent.layout

if layout == "tabbed":
    if num_splits > 0:
        i3.command(move_tabbed[sys.argv[1]])
    else:
        i3.command(move_tabbed_single[sys.argv[1]])
else:
    i3.command(move_normal[sys.argv[1]])
