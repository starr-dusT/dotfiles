from libqtile.config import Screen
from libqtile import bar, widget
from settings.widgets import primary_widgets

# Define the screens (and bars)
screens = [
    Screen(),
    Screen(top=bar.Bar(widgets=primary_widgets, size=20)),
]
