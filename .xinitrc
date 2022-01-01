#exec dbus-launch xmonad
#exec qtile start
#picom &
exec dbus-launch --exit-with-session emacs -mm --eval "(exwm-enable)"
#--debug-init
#exec stumpwm start
