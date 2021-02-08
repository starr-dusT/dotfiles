from os.path import expanduser
from yaml import load

# Get color config from pywal
wal_loc = expanduser("~/.config/qtile/themes/dracula.yml")
wal = load(open(wal_loc))
