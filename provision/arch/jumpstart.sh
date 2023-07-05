#!/usr/bin/env bash

echo -e "Starting initial setup for Arch..."

CHEZDIR="$HOME/.local/share/chezmoi"
echo "Input email for bitwarden:"
read bitemail

# Install ansible python dependencies
sudo pacman -Syu
sudo pacman python3 python3-pip ansible cargo -y
pip install pexpect
cargo install rbw

# Add things to path for this script
mkdir -p $HOME/.local/bin
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$CHEZDIR/temp_bin"
export PATH="$PATH:$HOME/.local/bin"

# Install ansible extensions
ansible-galaxy install -r "$CHEZDIR/provision/debian/ansible/requirements.yml"

# Run setup playbook
ansible-playbook "$CHEZDIR/provision/debian/ansible/setup.yml" -i "$CHEZDIR/provision/debian/ansible/hosts" --ask-become-pass

# Copy jumpstart scripts to temp bin dir and add to path
mkdir -p "$CHEZDIR/temp_bin"
cp "$CHEZDIR/home/bin/executable_rbw-get" "$CHEZDIR/temp_bin/rbw-get"
chmod +x "$CHEZDIR/temp_bin/rbw-get"

# Set bitwarden email
rbw config set email "$bitemail"

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
