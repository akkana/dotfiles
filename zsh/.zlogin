# .zlogin happens *after* .zshrc.
# Use .zprofile for anything that should happen first.

if [[ -f /etc/motd ]]; then
  cat /etc/motd
fi

# Things to do only when logged on to the console, so only once
# per session:
if [[ $(tty) == /dev/tty1 ]]; then
    # GTK needs this to pay attention to .XCompose bindings
    export GTK_IM_MODULE=xim

    # On the Dell Latitude 2120, beep (if it beeps at all) is stupidly loud.
    # It's controlled by a separate ALSA channel and defaults to 100%.
    # You can also mute it, with set Beep mute.
    if [[ $HOSTNAME == 'io' ]]; then
        amixer -q -c 0 set Beep 5
    fi

    ########### Environment #####################

    # Set path
    arch=$(uname -m)
    export PATH=$HOME/bin:$HOME/bin/linux-$arch:/usr/local/bin:$PATH:.:/usr/sbin:/sbin:$HOME/Archive/outsrc/android-sdk-linux/platform-tools:$HOME/Archive/outsrc/android-sdk-linux/tools:$HOME/.local/bin

    if (( ! ${+PYTHONPATH} )); then
        export PYTHONPATH=$HOME/bin/pythonlibs
    fi

    # Environment
    export PAGER=less
    # Need -er in LESS, for git colors to work
    export LESS="-EerX"
    export LC_COLLATE=C

    export MAILER=mutt
    export EDITOR=vim
    export VISUAL=vim

    # Set various XDG dirs that annoying programs might try to create unasked.
    # Full list, https://wiki.archlinux.org/index.php/XDG_user_directories
    export XDG_DESKTOP_DIR=/tmp
    export XDG_DOWNLOAD_DIR=/tmp
    # export XDG_DOCUMENTS_DIR=/tmp
    # export XDG_MUSIC_DIR=/tmp
    export XDG_PICTURES_DIR=/tmp
    export XDG_VIDEOS_DIR=/tmp

    # Autocomplete in the python console:
    # https://python.readthedocs.io/en/v2.7.2/tutorial/interactive.html
    if [[ -f ~/.pystartup ]]; then
        export PYTHONSTARTUP=~/.pystartup
    fi

    ########### End Environment

    # Start networking if we have a remembered scheme
    # if [[ -f $HOME/.config/netscheme/current ]]; then
    #   netscheme -w -r > /tmp/netscheme-login.out 2>&1 &
    # fi

    # Housekeeping, things we want to clean up regularly.
    # These are all directories created by programs I don't use voluntarily
    # or want to use in anonymous, no-tracking mode,
    # or keep reverting to their prefs rather than mine.
    echo "Cleaning up bogus directories"
    rm -rf .cache/chromium .cache/google-chrome .macromedia .vlc Downloads
    # and Firefox creates ~/Downloads even if that isn't set as the
    # download directory, idiotically
    if [[ -d ~/Downloads ]]; then
        rmdir ~/Downloads
    fi

    check-spam-blanks

    # Which hosts should start X automatically?
    autox=(hesiodus iridum)
    if [[ ${autox[(r)$hostname]} == $hostname ]]; then
        echo "Starting x ..."
        startx -- >& $HOME/.Xout
        # echo Starting X with dumb scheduler
        # startx -- -dumbSched >& $HOME/.Xout
        # -keeptty is documented as only for debugging, not supported everywhere
        # but it may be the only way to log stderr any more.
        # startx -- -keeptty >& $HOME/.Xout
        # startx -- -dumbSched vt$XDG_VTNR -keeptty >& $HOME/.Xout
    fi
    alias xx='startx >~/.Xout 2>&1'

else

    # If we're logged in over a serial port, we might be using screen
    # on something like a Raspberry Pi,
    # in which case we need to set the terminal size explicitly:
    if [[ $(tty) =~ /dev/ttyAMA0 ]]; then
        termsize
    fi
fi

if [[ -s ~/.reminders ]]; then
    echo "==================================================="
    cat ~/.reminders
fi


echo "==================================================="
rancmd() {
    echo "Random command of the day:"
    ranman=$(ls -1 /usr/share/man/man1/ /usr/share/man/man8 | shuf -n 1)
    echo "(from $ranman)"
    man -f $(echo $ranman | sed 's_\.[0-9].*__')
}
rancmd

if [[ -x /usr/bin/remind && -e ~/Docs/Lists/remind ]]; then
    echo "==================================================="
    remind -g ~/Docs/Lists/remind
fi

acpi
