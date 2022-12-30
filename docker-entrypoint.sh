#!/usr/bin/env bash
# This script stands in for the void-initial scripts that bootstraps void with
# some specific fixes to work within github workflows

# Install seed packages
whoami
sudo xpbs-install -Syu -y
sudo xbps-install python3 python3-pip ansible -y
sudo pip install pexpect github3.py

# Install roles and run playbook
LANG=en_US.UTF-8 ansible-galaxy install -r /home/tstarr/.local/share/chezmoi/provision/requirements.yml
cd /home/tstarr/.local/share/chezmoi/provision
LANG=en_US.UTF-8 ansible-playbook setup.yml -i hosts
