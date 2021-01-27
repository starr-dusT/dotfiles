from libqtile import layout
from settings.wal import wal

# Layout configs
layout_conf = {
    'border_focus': wal['colors']['color2'],
    'border_normal': wal['colors']['color0'],
    'border_width': 1,
    'margin': 2,
}

# Define the layouts I have
layouts = [
    layout.MonadTall(**layout_conf),
    layout.Max(**layout_conf),
    layout.Stack(num_stacks=2, **layout_conf),
    layout.MonadWide(**layout_conf),
    layout.Matrix(columns=2, **layout_conf),
    layout.Zoomy(**layout_conf),
    layout.Bsp(**layout_conf),
]

# Define floating rules
floating_layout = layout.Floating(
    float_rules=[
        {'wmclass': 'Steam'},
        {'wmclass': 'Wine'},
        {'wmclass': 'discord'},
    ],
    border_focus=wal['colors']['color2'],
    border_width=1,
)
