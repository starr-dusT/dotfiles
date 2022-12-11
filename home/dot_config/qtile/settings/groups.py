from libqtile.config import Key, Group, ScratchPad, DropDown, Match, hook
from libqtile.command import lazy
from settings.keys import mod, keys

# Define groups I have
groups = [Group(i) for i in [
    "1", "2", "3", "4", "5"
]]

# Define keybinds for groups
for i, group in enumerate(groups):
    actual_key = str(i + 1)
    keys.extend([
        # Switch to workspace N
        Key([mod], actual_key, lazy.group[group.name].toscreen()),
        # Send window to workspace N
        Key([mod, "shift"], actual_key, lazy.window.togroup(group.name))
    ])

groups.append(
    #Group("Comm", spawn="discord", persist=True)
    ScratchPad("Comm", [DropDown("d", "discord", match=Match(wm_class="discord"), x=0.1, y=0.1, width=0.8, height=0.8)])
)

keys.extend([
    Key([mod], "d", lazy.group["Comm"].dropdown_toggle("d"))
])

