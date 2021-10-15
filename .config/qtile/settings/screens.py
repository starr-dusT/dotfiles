from libqtile.config import Screen
from libqtile import bar, widget
from settings.widgets import primary_widgets

# Define the screens (and bars)
screens = [
    Screen(top=bar.Bar(widgets=primary_widgets, size=20)),
    Screen(top=bar.Bar([
            widget.GroupBox(),
            widget.WindowName()
            ], 20),)
]
