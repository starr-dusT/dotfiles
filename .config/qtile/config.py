from typing import List  # noqa: F401
from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
import os
import subprocess

# Use the "Windows" key for mod
mod = "mod4"
# Determine terminal with this nifty function
terminal = guess_terminal()
# Define workspace names
groups = [Group(i) for i in "123456789"]
# Generate group binding hotkeys
dgroups_key_binder = None
# List of rule objects (currently empty)
dgroups_app_rules = []
# Set mouse functionality
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False
# Set fullscreen options
auto_fullscreen = True
focus_on_window_activation = "smart"
# Set floating layout options
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'Wine'},
    {'wmclass': 'Steam'},
])

# Start keys block
keys = [
    # Close focused window
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    # Rotate through the available layout algorithms
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(),
        desc="Move window focus to other window"),
    # Navigate between windows on screen
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),
    # Resize windows on screen
    Key([mod, "shift"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "shift"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "shift"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "shift"], "k", lazy.layout.grow_up(),
        desc="Grow window up"),
    # Return sizes to default
    Key([mod], "n", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    # Toggle between physical screens
    Key([mod], "comma", lazy.to_screen(0),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "period", lazy.to_screen(1),
        desc="Toggle between split and unsplit sides of stack"),
    # Toggle fullscreen
    Key([mod], "f", lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen"),
    # Toggle floating
    Key([mod], "t", lazy.window.toggle_floating(),
        desc="Toggle floating"),

    # Spawn terminal
    Key([mod], "Return", lazy.spawn(terminal),
        desc="Launch terminal"),
    # Spawn rofi drun
    Key([mod], "w", lazy.spawn("rofi -show drun"),
        desc="Launch rofi -drun"),
    # Spawn rofi window
    Key([mod, "shift"], "w", lazy.spawn("rofi -show window"),
        desc="Launch rofi -window"),

    # Recompile and restart qtile
    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    # Quit qtile
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    # Qtile's in-built launcher
    Key([mod], "r", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),
    # Start gamemoded
    Key([mod], "g", lazy.spawn("gamemoded -r"),
        desc="Start gamemode"),
    # Stop gamemoded
    Key([mod, "shift"], "g", lazy.spawn("killall gamemoded"),
        desc="Stop gamemode"),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

layouts = [
    layout.Columns(border_focus_stack='#d75f5f', margin=2),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='JetBrains Mono Nerd Font',
    fontsize=11,
    padding=3,
)
extension_defaults = widget_defaults.copy()

colors = [["#282c34", "#282c34"], # panel background
          ["#434758", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#8d62a9", "#8d62a9"], # border line color for other tab and odd widgets
          ["#668bd7", "#668bd7"], # color for the even widgets
          ["#e1acff", "#e1acff"]] # window name

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    fontsize = 9,
                    margin_y = 3,
                    margin_x = 0,
                    padding_y = 5,
                    padding_x = 3,
                    borderwidth = 3,
                    active = colors[2],
                    inactive = colors[2],
                    rounded = False,
                    highlight_color = colors[1],
                    highlight_method = "line",
                    this_current_screen_border = colors[3],
                    this_screen_border = colors [4],
                    other_current_screen_border = colors[0],
                    other_screen_border = colors[0],
                    foreground = colors[2],
                    background = colors[0],
                    hide_unused=True),
                widget.TextBox('|'),
                widget.WindowName(),
                widget.Prompt(),


                widget.TextBox('|'),
                widget.CPUGraph(frequency=1, samples=30, type='box'),
                widget.TextBox('|'),
                widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
            ],
            24,
        ),
    ),
]

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])
