from os.path import expanduser
from json import load

# Get color config from pywal
wal_loc = expanduser('~/.cache/wal/colors.json')
wal = load(open(wal_loc))
