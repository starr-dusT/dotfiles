from libqtile import widget
from settings.wal import wal

spacer_len = 3

widget_defaults = dict(
    font='JetBrains Mono Nerd Font',
    fontsize=10,
    padding=3,
    background = wal['colors']['color0']
)
extension_defaults = widget_defaults.copy()

primary_widgets = [
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
]
