#!/bin/bash

# Get and stow dotfiles
sudo pacman -S stow
sudo pacman -S git
git clone https://github.com/starr-dusT/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
stow .

# Get aconfmgr and apply
mkdir -p ~/.git
git clone https://github.com/CyberShadow/aconfmgr.git ~/.git/aconfmgr
cd ~/.git/aconfmgr
./aconfmgr apply
