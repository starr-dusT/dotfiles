
#!/usr/bin/env sh

# Terminate already running bar instances
pkill clipmenud

# Wait until the processes have been shut down
while pgrep -x clipmenud >/dev/null; do sleep 1; done

# Launch clipmenud
clipmenud &
