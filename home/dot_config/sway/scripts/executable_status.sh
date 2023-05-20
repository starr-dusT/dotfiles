# The Sway configuration file in ~/.config/sway/config calls this script.
# You should see changes to the status bar after saving this script.

# Uptime 
uptime_formatted=$(uptime | cut -d ',' -f1  | cut -d ' ' -f7)

# Date
date_formatted=$(date "+%a %F %H:%M")

# Kernel Version 
linux_version=$(uname -r)

# Gamemode status
status=$(gamemoded -s)
if [ "$status" == "gamemode is inactive" ]; then
	gamemode=🧊
else
	gamemode=🔥
fi

# Volume
sink=$( pactl list short sinks | sed -e 's,^\([0-9][0-9]*\)[^0-9].*,\1,' | head -n 1 )
volume=$( pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,' )
mute=$(pactl list sinks | grep '^[[:space:]]Mute:' | head -n $(( $SINK + 1 )) | tail -n 1 | awk '{print $2}')

if [ "$mute" == "yes" ]; then
    volume_color='#f92672'
else
    volume_color='#ffffff'
fi

#bluetooth=$(bluetoothctl devices | cut -f2 -d' ' | while read uuid; do bluetoothctl info $uuid; done | grep -e "Name\|Connected: yes" | grep -B1 "yes" | head -n 1 | cut -d\  -f2-)
bluetooth="a"

#<span foreground='#c16b26'>lel</span>
echo -e "🫐 $bluetooth | ⬆️ $uptime_formatted | 🔉<span foreground='$volume_color'>$volume%</span> | $gamemode | 🐧 $linux_version | $date_formatted "
