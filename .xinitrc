#!/bin/sh

if [ -d "$HOME/.local/bin" ] ; then
  PATH="$PATH:$HOME/.local/bin"
fi

#xrdb -load ~/.Xresources
exec dbus-launch xmonad
#exec qtile start
#exec dbus-launch --exit-with-session emacs -mm --eval "(exwm-enable)"
#exec herbstluftwm --locked
#export GDK_CORE_DEVICE_EVENTS=1
#exec stumpwm
