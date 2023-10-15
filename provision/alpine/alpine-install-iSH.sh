#!/bin/sh
# https://github.com/starr-dusT/dotfiles

# Install edge repo
echo "https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
apk update

# Install packages
apk add --virtual .packs \
    task timewarrior git \
    fzf neovim nnn rbw \
    chezmoi openssh bash

# Clone dotfiles
git clone https://github.com/starr-dusT/dotfiles.git ~/.local/share/chezmoi 

# Move binaries from alpine pkgs
mkdir -p ~/bin
mv ~/.local/share/chezmoi/provision/alpine/pkgs/* ~/bin 
