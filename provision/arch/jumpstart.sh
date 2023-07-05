#!/usr/bin/env bash

echo -e "Starting initial setup for Arch..."

CHEZDIR="$HOME/.local/share/chezmoi"
echo "Input email for bitwarden:"
read bitemail

# Install ansible python dependencies
sudo pacman -Syu
sudo pacman -Syu python3 python-pip ansible cargo
sudo find / -name "EXTERNALLY-MANAGED" -type f -delete
pip install pexpect
cargo install rbw

# Add things to path for this script
mkdir -p $HOME/.local/bin
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$CHEZDIR/temp_bin"
export PATH="$PATH:$HOME/.local/bin"

# Install ansible extensions
ansible-galaxy install -r "$CHEZDIR/provision/arch/ansible/requirements.yml"

# Run setup playbook
ansible-playbook "$CHEZDIR/provision/arch/ansible/setup.yml" -i "$CHEZDIR/provision/arch/ansible/hosts" --ask-become-pass

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
