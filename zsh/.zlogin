# .zlogin happens *after* .zshrc.
# Use .zprofile for anything that should happen first.

if [[ -f /etc/motd ]]; then
  cat /etc/motd
fi

# Things to do only when logged on to the console, so only once
# per session:
if [[ $(tty) == /dev/tty1 ]]; then
    # GTK needs this to pay attention to .XCompose bindings.
    # Except maybe it doesn't anyway.
#    export GTK_IM_MODULE=xim

    # On the Dell Latitude 2120, beep (if it beeps at all) is stupidly loud.
    # It's controlled by a separate ALSA channel and defaults to 100%.
    # You can also mute it, with set Beep mute.
    if [[ $HOSTNAME == 'io' ]]; then
        amixer -q -c 0 set Beep 5
    fi

    ########### Environment #####################

    # Set path
    arch=$(uname -m)
    export PATH=$HOME/bin:$HOME/bin/linux-$arch:/usr/local/bin:$PATH:.:/usr/sbin:/sbin:$HOME/.local/bin

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
    # This works for Zoom but, sadly, not for firefox.
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

    umask 22

    # Start networking if we have a remembered scheme
    # if [[ -f $HOME/.config/netscheme/current ]]; then
    #   netscheme -w -r > /tmp/netscheme-login.out 2>&1 &
    # fi

    # Housekeeping, things we want to clean up regularly.
    # These are all directories created by programs I don't use voluntarily
    # or want to use in anonymous, no-tracking mode,
    # or keep reverting to their prefs rather than mine.
    # Firefox in particular loves to create Downloads and Desktop
    # even if you've specifically set the download directory otherwise.
    echo "Cleaning up bogus directories"
    rm -rf .cache/chromium .cache/google-chrome .macromedia .vlc Downloads Desktop Videos ~/Templates

    # check-spam-blanks

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

    if [[ $(hostname) == 'charon' ]]; then
        pulsehelper --source none --sink 'Cannon Point Speaker'
        # Incantation to enable all four speakers on Carbon X1 Gen 7
        # https://gist.github.com/hamidzr/dd81e429dc86f4327ded7a2030e7d7d9
        # https://forums.lenovo.com/t5/Ubuntu/Guide-X1-Carbon-7th-Generation-Ubuntu-compatability/m-p/4489823?page=15#5085965
        sudo /usr/bin/hda-verb /dev/snd/hwC0D0 0x17 SET_CONNECT_SEL 1
    fi

    # End things to do only on tty1
else

    # If we're logged in over a serial port, we might be using screen
    # on something like a Raspberry Pi,
    # in which case we need to set the terminal size explicitly:
    if [[ $(tty) =~ /dev/ttyAMA0 ]]; then
        termsize
    fi
fi

# Macs don't have rxvt-unicode-256color, and ssh from urxvt will
# result in all sorts of weird problems in commandline editing,
# like echoing spaces instead of deleting characters on backspace.
# chishio (which hosts lwvnm) doesn't have it either.
# XXX Might be better to check existence of
# /usr/lib/terminfo/r/rxvt-unicode-256color
# if [[ $(uname) == Darwin && $TERM == rxvt-unicode-256color ]]; then
if [[ ! -e /usr/lib/terminfo/r/rxvt-unicode-256color ]]; then
    export TERM=rxvt
fi

export XDG_UTILS_DEBUG_LEVEL=99

if [[ $(hostname) == 'charon' ]]; then
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
fi

if [[ -s ~/.reminders ]]; then
    echo "==================================================="
    cat ~/.reminders
fi
