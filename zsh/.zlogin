# .zlogin happens *after* .zshrc.
# Use .zprofile for anything that should happen first.

if [[ -f /etc/motd ]]; then
  cat /etc/motd
fi

if [[ $(tty) == /dev/tty1 ]]; then
    # GTK needs this to pay attention to .XCompose bindings
    export GTK_IM_MODULE=xim

    # On the Dell Latitude 2120, beep (if it beeps at all) is stupidly loud.
    # It's controlled by a separate ALSA channel and defaults to 100%.
    # You can also mute it, with set Beep mute.
    if [[ $HOSTNAME == 'io' ]]; then
        amixer -q -c 0 set Beep 5
    fi

    # Start networking if we have a remembered scheme
    if [[ -f $HOME/.config/netscheme/current ]]; then
      netscheme -w -r > /tmp/netscheme-login.out 2>&1 &
    fi

    # Housekeeping, things we want to clean up regularly
    rm -rf .cache/chromium .cache/google-chrome .macromedia .vlc

    # echo Starting X with dumb scheduler
    echo "Starting x ..."
    startx -- >& $HOME/.Xout
    # startx -- -dumbSched >& $HOME/.Xout
    # -keeptty is documented as only for debugging and not supported everywhere
    # but it may be the only way to log stderr any more.
    # startx -- -keeptty >& $HOME/.Xout
    # startx -- -dumbSched vt$XDG_VTNR -keeptty >& $HOME/.Xout
else
    if [[ -x /usr/bin/remind && -e ~/Docs/Lists/remind ]]; then
        remind -g ~/Docs/Lists/remind
    fi

    echo "==================================================="

    if [[ -f ~/.reminders ]]; then
        cat ~/.reminders
    fi
fi

# export ZMLIB=~/outsrc/zmail-2009/zmail/lib

# By default, use a python2 virtualenv.
# It might be possible to use both, even though most people say not to;
# more research required.
echo
python2env
