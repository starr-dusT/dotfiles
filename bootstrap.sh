#!/bin/bash

# Get and stow dotfiles
sudo pacman -S stow
stow .

# Get aconfmgr and apply
mkdir -p ~/.git
git clone https://github.com/CyberShadow/aconfmgr.git ~/.git/aconfmgr
cd ~/.git/aconfmgr
./aconfmgr apply
