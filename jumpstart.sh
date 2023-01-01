#!/usr/bin/env bash

echo -e "Starting initial setup..."

CHEZDIR="/home/test/.local/share/chezmoi"
echo "Input email for bitwarden:"
read bitemail

# Install ansible python dependencies
sudo xbps-install -Syu -y
sudo xbps-install python3 python3-pip ansible -y
sudo pip install pexpect

# Install ansible extensions
ansible-galaxy install -r "$CHEZDIR/provision/requirements.yml"

# Run setup playbook
ansible-playbook "$CHEZDIR/provision/setup.yml" -i "$CHEZDIR/provision/hosts" --ask-become-pass

export PATH="$PATH:/usr/local/bin"

# Copy jumpstart scripts to temp bin dir and add to path
mkdir -p "$CHEZDIR/temp_bin"
cp "$CHEZDIR/home/bin/executable_rbw-get" "$CHEZDIR/temp_bin/rbw-get"
chmod +x "$CHEZDIR/temp_bin/rbw-get"
export PATH="$PATH:$CHEZDIR/temp_bin"

# Set bitwarden email
rbw config set email "$bitemail"

# Make temporary i3 gen file
mkdir -p "$CHEZDIR/home/.gen"
echo "{
    \"disp_pri\": \"HDMI-0\",
    \"disp_sec\": \"HDMI-0\"
}" > "$CHEZDIR/home/.gen/i3.json"

# initialize chezmoi
chezmoi init

# first chezmoi apply
chezmoi apply

# reboot!
read -p "You should reboot, but I won't make you. Reboot? " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

sudo reboot

