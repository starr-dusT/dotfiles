from typing import List  # noqa: F401
from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
import os, subprocess, json

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
bring_front_click = False
cursor_warp = False
# Set fullscreen options
auto_fullscreen = True
focus_on_window_activation = "smart"
# Set floating layout options
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'Wine'},
    {'wmclass': 'Steam'},
])

wal_loc = os.path.expanduser('~/.cache/wal/colors.json')
wal = json.load(open(wal_loc))
spacer_len = 3

# Start keys block
keys = [
    # Close focused window
    Key([mod], "q",
        lazy.window.kill(),
    ),

    # Rotate through the available layout algorithms
    Key([mod], "space",
        lazy.next_layout(),
    ),
    Key([mod, "shift"], "space",
        lazy.prev_layout(),
    ),
    # Switch between windows
    Key([mod], "h",
        lazy.layout.left(),
    ),
    Key([mod], "l",
        lazy.layout.right(),
    ),
    Key([mod], "j",
        lazy.layout.down(),
    ),
    Key([mod], "k",
        lazy.layout.up(),
    ),

    # Move windows on screen
    Key([mod, "shift"], "h",
        lazy.layout.shuffle_left(),
        lazy.layout.client_to_next(),
        lazy.layout.flip_left(),
    ),
    Key([mod, "shift"], "l",
        lazy.layout.shuffle_right(),
        lazy.layout.client_to_previous(),
        lazy.layout.flip_right(),
    ),
    Key([mod, "shift"], "j",
        lazy.layout.shuffle_down(),
        lazy.layout.flip_down(),
    ),
    Key([mod, "shift"], "k",
        lazy.layout.shuffle_up(),
        lazy.layout.flip_up(),
    ),
    # Resize windows on screen
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        lazy.layout.shrink()
    ),
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        lazy.layout.grow()
    ),
    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
    ),
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
    ),
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
    layout.Columns(border_focus = wal['colors']['color2'],
                   border_normal = wal['colors']['color0'], margin = 2),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='JetBrains Mono Nerd Font',
    fontsize=10,
    padding=3,
    background = wal['colors']['color0']
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                # Group Box
                widget.GroupBox(active = wal['colors']['color0'],
                                inactive = wal['colors']['color0'],
                                urgent_text = wal['colors']['color0'],
                                background = wal['colors']['color1'],
                                this_current_screen_border = wal['colors']['color2'],
                                other_screen_border = wal['colors']['color2'],
                                urgent_border = wal['colors']['color6'],
                                highlight_method='block',
                                rounded = False,
                                hide_unused=True),
                 widget.TextBox(text='\uE0B0',
                                fontsize=17,
                                padding=0,
                                foreground=wal['colors']['color1']),
                # Window Name
                widget.Spacer(length = spacer_len,
                              background = wal['colors']['color0']),
                widget.WindowName(),
                # Prompt
                widget.Prompt(),
                # Volume
                widget.TextBox(text = '\uE0B2', fontsize = 17,
                               padding = 0, foreground = wal['colors']['color2'],
                               background = wal['colors']['color0']),
                widget.TextBox(text = ' ', fontsize = 14,
                               foreground = wal['colors']['color0'],
                               background = wal['colors']['color2']),
                widget.PulseVolume(foreground = wal['colors']['color0'],
                                   background = wal['colors']['color2']),
                widget.Spacer(length = spacer_len,
                              background = wal['colors']['color2']),
                # Clock
                widget.TextBox(text = '\uE0B2', fontsize = 17,
                               padding = 0, foreground = wal['colors']['color1'],
                               background = wal['colors']['color2']),
                widget.Clock(format='%Y-%m-%d %a %I:%M %p',
                             background = wal['colors']['color1'],
                             foreground = wal['colors']['color0']),
                widget.Spacer(length = spacer_len,
                              background = wal['colors']['color1']),
                widget.CurrentLayout(scale=0.6),
            ],
            20,
        ),
    ),
]

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])
