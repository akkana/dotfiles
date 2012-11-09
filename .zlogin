# .zlogin happens *after* .zshrc.
# Use .zprofile for anything that should happen first.
if [[ $(tty) == /dev/tty1 ]]; then
    # On the Dell Latitude 2120, beep is stupidly loud.
    # It's controlled by a separate ALSA channel and defaults to 100%.
    # You can also mute it, with set Beep mute.
    if [[ $HOSTNAME == 'io' ]]; then
        amixer -q -c 0 set Beep 5
    fi

    echo Starting X
    startx >& $HOME/.xsession-errors
elif [[ -x /usr/bin/remind && -e ~/Docs/Lists/remind ]]; then
    remind ~/Docs/Lists/remind
fi

export ZMLIB=~/outsrc/zmail-2009/zmail/lib

