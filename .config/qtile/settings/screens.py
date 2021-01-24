from libqtile.config import Screen
from libqtile import bar
from settings.widgets import primary_widgets

# Define the screens (and bars)
screens = [
    Screen(top=bar.Bar(primary_widgets, size=20))
]
