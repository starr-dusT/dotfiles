#
# ~/.bashrc

export PATH=$PATH:~/.cabal/bin
#export PATH=$PATH:~/.local/bin
alias lol="sudo sh -c 'sysctl -w abi.vsyscall32=0'"
alias dired="emacsclient -c -nw -a '' --eval '(dired nil)'"
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"
alias update-xmonad="yay -S xmonad-git xmonad-contrib-git xmobar-git"
alias git-python='/usr/bin/git --git-dir=$HOME/devel/python/python-bits-and-bobs --work-tree=$HOME/devel/python'

alias rm='echo "This is not the command you are looking for."; false'
alias tp='trash-put'
alias te='trash-empty'
alias tl='trash-list'
alias tre='trash-restore'
alias tpm='trash-rm'

alias mpv='mpv --title="mpv"'

alias dot='/usr/bin/git --git-dir=$HOME/.dots --work-tree=$HOME'
dot config --local status.showUntrackedFiles no

#export PATH="$PATH:$(ruby -e 'print Gem.user_dir')/bin"
#export GEM_HOME=$(ruby -e 'print Gem.user_dir')

[ -f "/home/tstarr/.ghcup/env" ] && source "/home/tstarr/.ghcup/env" # ghcup-env
