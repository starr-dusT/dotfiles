from os.path import expanduser
from yaml import safe_load

# Get color config from pywal
wal_loc = expanduser("~/.config/qtile/themes/monokai.yml")
wal = safe_load(open(wal_loc))
