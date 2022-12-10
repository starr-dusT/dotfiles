#!/usr/bin/python
#
import subprocess

def run_command(cmd, capture=False):
    if capture:
        return subprocess.getoutput(cmd)
    else:
        subprocess.run(cmd, shell=True)
        return 

# Get current ip address
ip_addr = run_command("hostname -I", True)
ip_slice = ip_addr.split(".")[2]

# Set monitor configuration based on ip address
if ip_slice == "1":
    run_command("xrandr --output HDMI-0 --mode 2560x1440 --pos 0x0 --rate 144 --output DP-4 --mode 2560x1440 --pos 2561x0 --rate 144 --primary --right-of HDMI-0")
elif ip_slice == "2":
    run_command("xrandr --output HDMI-0 --mode 2560x1440 --pos 0x0 --rate 144 --primary --output DP-4 --mode 2560x1440 --pos 2561x0 --rate 144 --right-of HDMI-0")
