#!/usr/bin/env python

import i3ipc

i3 = i3ipc.Connection()

def on_window_event(i3, e):
    focused_container = i3.get_tree().find_focused().parent
    # Get parent layout of focused window
    parent_layout = focused_container.layout
    # Get number of windows in focues container
    num_parent_windows = len(focused_container.nodes) 
    # Get number of windows in workspace
    workspace = i3.get_tree().find_focused().workspace()
    num_workspace_windows = len(workspace.leaves())
    if num_parent_windows > 1 and parent_layout != "tabbed":
        i3.command("splitv; layout tabbed")
    elif num_workspace_windows == 1:
        i3.command("layout splith")

# Subscribe to window events
i3.on("window", on_window_event)

# Start the main loop
i3.main()
