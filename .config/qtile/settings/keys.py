from libqtile.config import EzKey
from libqtile.command import lazy
from libqtile import qtile

# Set mod key to the "windows" key
mod = "mod4"

def window_to_previous_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group)


def window_to_next_screen(qtile):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group)

def switch_screens(qtile):
    i = qtile.screens.index(qtile.current_screen)
    group = qtile.screens[i - 1].group
    qtile.current_screen.set_group(group)

# Define keybinds
keys = [EzKey(k[0], *k[1:]) for k in [

    # ------ Movement ------ #
    # Navigate between windows
    ("M-h", lazy.layout.left()),
    ("M-j", lazy.layout.down()),
    ("M-k", lazy.layout.up()),
    ("M-l", lazy.layout.right()),
    # Switch windows
    ("M-S-<space>", lazy.function(switch_screens)),
    # Switch focus between two screens
    ("M-<period>", lazy.next_screen()),
    ("M-<comma>", lazy.prev_screen()),
    ("M-S-<period>", lazy.function(window_to_next_screen)),
    ("M-S-<comma>", lazy.function(window_to_previous_screen)),
    # Move windows around
    ("M-S-h", lazy.layout.shuffle_left(),
              lazy.layout.swap_left()),
    ("M-S-j", lazy.layout.shuffle_down()),
    ("M-S-k", lazy.layout.shuffle_up()),
    ("M-S-l", lazy.layout.shuffle_right(),
              lazy.layout.swap_right()),
    # Resize windows
    ("M-C-h", lazy.layout.grow_left().when('bsp'),
              lazy.layout.shrink().when(['monadtall', 'monadwide'])),
    ("M-C-j", lazy.layout.grow_down().when('bsp')),
    ("M-C-k", lazy.layout.grow_up().when('bsp')),
    ("M-C-l", lazy.layout.grow_right().when('bsp'),
              lazy.layout.grow().when(['monadtall', 'monadwide'])),
    ("M-C-n", lazy.layout.normalize()),
    # Swap master and stack
    ("M-<space>", lazy.layout.flip().when(['monadtall', 'monadwide']),
                  lazy.layout.rotate().when(['stack'])),

    # ------ Window State Changes ------ #
    # Kill focused window
    ("M-q", lazy.window.kill()),
    # Toggle Floating
    ("M-t", lazy.window.toggle_floating()),

    # ------ Progam Launching ------ #
    # Program launcher
    ("M-w", lazy.spawn("rofi -show drun")),
    ("M-S-w", lazy.spawn("rofi -show window")),
    # Open Programs
    ("M-<Return>", lazy.spawn("alacritty")),
    ("M-S-<Return>", lazy.spawn("alacritty -e vifm")),
    ("M-b", lazy.spawn("brave")),
    ("M-d", lazy.spawn("discord")),
    ("M-e", lazy.spawn("emacs")),
    ("M-g", lazy.spawn("lutris")),

    # ------ System + Utils ------- #
    # Resart qtile
    ("M-S-r", lazy.restart()),
    # Quit qtile
    ("M-S-q", lazy.shutdown()),
    # Switch between layouts
    ("M-<Tab>", lazy.next_layout()),
    ("M-S-<Tab>", lazy.prev_layout()),
    # Screenshot
    ("M-s", lazy.spawn("flameshot gui")),
    # Gamemode
    ("M-S-g", lazy.spawn('toggle_gamemode')),
    # Manage computer audio
    ("<XF86AudioLowerVolume>",
     lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -2%")),
    ("<XF86AudioRaiseVolume>",
     lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +2%")),
    ("<XF86AudioMute>",
     lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),

]]
