#!/bin/sh
# https://github.com/starr-dusT/dotfiles

# Install edge repo
echo "https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
apk update

# Install packages
apk add --virtual .packs \
    task timewarrior git go \
    make fzf neovim nnn rbw \
    chezmoi openssh bash

# Build and install zk
mkdir -p ~/tmp
mkdir -p ~/bin
git clone https://github.com/mickael-menu/zk.git ~/tmp/zk
cd ~/tmp/zk 
make
mv zk ~/bin/zk

# Clone dotfiles
git clone https://github.com/starr-dusT/dotfiles.git ~/.local/share/chezmoi 
