#
# ~/.bashrc
#

alias lol="sudo sh -c 'sysctl -w abi.vsyscall32=0'"
alias dired="emacsclient -c -nw -a '' --eval '(dired nil)'"
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"
alias update-xmonad="yay -S xmonad-git xmonad-contrib-git xmobar-git"
alias git-python='/usr/bin/git --git-dir=$HOME/devel/python/python-bits-and-bobs --work-tree=$HOME/devel/python'


alias dot='/usr/bin/git --git-dir=$HOME/.dots --work-tree=$HOME'
dot config --local status.showUntrackedFiles no


export PATH="$PATH:$(ruby -e 'print Gem.user_dir')/bin"
export GEM_HOME=$(ruby -e 'print Gem.user_dir')




# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

#xsetroot -cursor_name left_ptr



