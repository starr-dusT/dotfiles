#!/usr/bin/python

import os
import subprocess
import sys

input_str = sys.stdin.read().replace("\n", "")
pkg = input_str.replace(" ", "")[1:]
cmd = "xbps-query -Rs %s"%(pkg)
output = subprocess.getoutput(cmd)
if output.count('[') > 1:
    cmd = "xbps-query -Rs %s | grep -E ' %s-[0-9](.|[0-9])[0-9]'"%(pkg,pkg)
    output = subprocess.getoutput(cmd)
upper_pos = -1
for index, char in enumerate(output):
    if char == char.upper() and char.isalpha():
        upper_pos = index
        break
if upper_pos == -1:
    sys.stdout.write("error")
else:
    sys.stdout.write(input_str + " # " + output[upper_pos:])
