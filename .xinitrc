# .xinitrc for Openbox

# All the things I used to run here have moved to ~/.config/openbox/autostart.
# dbus-launch doesn't play well with anything left here.

#openbox

#/usr/bin/dbus-launch --exit-with-session openbox-session

# Here's what Mikachu says he does:
# eval $(dbus-launch --sh-syntax)
# But then you can get extraneous dbus daemons running,
# since they don't exit with the X session.
# The advantage is that if openbox crashes (which might happen if you're
# testing experimental openbox features), your session dbus doesn't
# crash with it -- though it looks like dbus --exit-with-session might
# actually be tied to the X session, not to the program it runs directly.

# But this also seems to work:
/usr/bin/dbus-launch --exit-with-session openbox --startup $HOME/.config/openbox/autostart
