from libqtile import widget
from settings.wal import wal

spacer_len = 3
wal_color = wal["colors"]

widget_defaults = dict(
    font="JetBrains Mono Nerd Font",
    fontsize=10,
    padding=3,
    background=wal_color["color0"],
)
extension_defaults = widget_defaults.copy()

primary_widgets = [
    widget.GroupBox(
        urgent_border=wal_color["color0"],
        disable_drag=True,
        highlight_method="block",
        this_screen_border=wal_color["color6"],
        other_screen_border=wal_color["color2"],
        this_current_screen_border=wal_color["color6"],
        other_current_screen_border=wal_color["color2"],
        background=wal_color["color1"],
        hide_unused=True,
        visible_groups=["1", "2", "3", "4", "5"],
    ),
    widget.TextBox(
        text="\uE0B0",
        fontsize=17,
        padding=0,
        background=wal_color["color2"],
        foreground=wal_color["color1"],
    ),
    # Layout Name
    widget.CurrentLayout(
        scale=0.6, foreground=wal_color["color0"], background=wal_color["color2"]
    ),
    widget.TextBox(
        text="\uE0B0",
        fontsize=17,
        padding=0,
        background=wal_color["color1"],
        foreground=wal_color["color2"],
    ),
    # Window count
    widget.WindowCount(
        scale=0.6, foreground=wal_color["color0"], background=wal_color["color1"]
    ),
    widget.TextBox(
        text="\uE0B0", fontsize=17, padding=0, foreground=wal_color["color1"]
    ),
    # Window Name
    widget.Spacer(length=spacer_len),
    widget.WindowName(foreground=wal_color["color2"]),
    # System Tray
    widget.Systray(background=wal_color["color0"], padding=0),
    widget.Spacer(length=spacer_len, background=wal_color["color0"]),
    # Cpu
    widget.TextBox(
        text="\uE0B2",
        fontsize=17,
        padding=0,
        foreground=wal_color["color2"],
        background=wal_color["color0"],
    ),
    widget.CPU(
        format="CPU {freq_current}GHz {load_percent}%",
        update_interval=1.0,
        foreground=wal_color["color0"],
        background=wal_color["color2"],
        padding=5,
    ),
    # Network
    widget.TextBox(
        text="\uE0B2",
        fontsize=17,
        padding=0,
        foreground=wal_color["color1"],
        background=wal_color["color2"],
    ),
    widget.Net(
        interface="enp4s0",
        format="{down}  ↓↑ {up}",
        foreground=wal_color["color0"],
        background=wal_color["color1"],
        padding=5,
    ),
    # Volume
    widget.TextBox(
        text="\uE0B2",
        fontsize=17,
        padding=0,
        foreground=wal_color["color2"],
        background=wal_color["color1"],
    ),
    widget.TextBox(
        text=" ",
        fontsize=14,
        foreground=wal_color["color0"],
        background=wal_color["color2"],
    ),
    widget.PulseVolume(foreground=wal_color["color0"], background=wal_color["color2"]),
    widget.Spacer(length=spacer_len, background=wal_color["color2"]),
    # Clock
    widget.TextBox(
        text="\uE0B2",
        fontsize=17,
        padding=0,
        foreground=wal_color["color1"],
        background=wal_color["color2"],
    ),
    widget.Clock(
        format="%Y-%m-%d %a %I:%M %p",
        background=wal_color["color1"],
        foreground=wal_color["color0"],
    ),
    widget.Spacer(length=spacer_len, background=wal_color["color1"]),
]
