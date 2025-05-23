# .zlogin happens *after* .zshrc.
# Use .zprofile for anything that should happen first.

# When debugging problems:
# echo '============================== .zlogin'
# set -x

if [[ -f /etc/motd ]]; then
  cat /etc/motd
fi

# Things to do only when logged on to the console, so only once
# per session. Under X, that will happen on tty1.
# But if SSH_TTY is set then we're ssh'ed and can't rely on tty1.
if [[ $(tty) == /dev/tty1 || ! -z "$SSH_TTY" ]]; then
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
    export PATH=$HOME/bin:$HOME/bin/linux-$arch:/usr/local/bin:$PATH:/usr/sbin:/sbin:$HOME/.local/bin

    if (( ! ${+PYTHONPATH} )); then
        export PYTHONPATH=$HOME/bin/pythonlibs
    fi

    # Environment
    export PAGER=less

    # Use quickbrowse for email attachments
    export EMAIL_BROWSER=quickbrowse

    # Need -er in LESS, for git colors to work.
    # But -er makes less fail to consider long lines.
    # export LESS="-EerX"
    # -e: exit the second time it sees EOF. -E: first time.
    # -R: let ANSI color sequences through
    # -X disables sending the terminal initialization sequence,
    # meaning it disables using the alt screen.
    export LESS="-eRX"

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

    # Debian's python3-pyproj package can't be bothered to be aware
    # of its own data directory, so it gives an annoying warning
    # "Valid PROJ data directory not found"
    # every time it's run unless you do this:
    export PROJ_DATA=/usr/share/proj

    # Autocomplete in the python console:
    # https://python.readthedocs.io/en/v2.7.2/tutorial/interactive.html
    if [[ -f ~/.pystartup ]]; then
        export PYTHONSTARTUP=~/.pystartup
    fi

    export EDITOR=vim

    # If EDITOR is vim, zsh will try to be "smart" and switch to vi mode.
    # This switches bindings back to emacs:
    bindkey -e

    export RSYNC_RSH=ssh
    # export PHO_ARGS=-p

    # Disable NoCSD for GTK
    # if [[ -e /usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0 ]]; then
    #     export GTK_CSD=0
    #     export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
    # fi
    # if [[ -e /usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0 ]]; then
    #     export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libgtk3-nocsd.so.0
    # fi

    # virt-manager by default makes sessions that are only for root,
    # while virsh looks for a QEMU/KVM user session.
    # Tell virsh to use the root session:
    export LIBVIRT_DEFAULT_URI="qemu:///system"

    # ls colors, see:
    # https://gist.github.com/jmoz/280005/3dca508fb193b6ae5d1f4a3f21efc7d90ecb0bde
    # http://www.bigsoft.co.uk/blog/index.php/2008/04/11/configuring-ls_colors
    # https://web.archive.org/web/20201129214825/http://www.linux-sxs.org/housekeeping/lscolors.html
    export LS_COLORS='ex=1;31:ln=1;35:ow=47;1:or=0;103;1'

    # Allow personal ispell dictionary without cluttering ~
    export WORDLIST=~/.config/spell/ispell_words

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
    rm -rf .cache/chromium .cache/google-chrome .macromedia .vlc Downloads Desktop Videos ~/Templates "~/Calibre Library" ~/.local/share/calibre-ebook.com
    # Might want to add lots more directories in ~/.local/share
    # as well as chromium directories scattered all over the filesystem
    # rm -rf snap/chromium/common/.cache/

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

    # if [[ $(hostname) == 'charon' ]]; then
    #     # mute the microphone, set the builtin speakers as default output
    #     # This works from a shell after logging in, but at login time
    #     # it doesn't.
    #     echo "**** DEVICES:" > /tmp/pulsehelper.login
    #     pulsehelper >> /tmp/pulsehelper.login 2>&1
    #     echo "**** pacmd list-sinks:" >> /tmp/pulsehelper.login
    #     pacmd list-sinks >> /tmp/pulsehelper.login 2>&1
    #     echo "**** Setting:" >> /tmp/pulsehelper.login
    #     pulsehelper --source none --sink spkr >> /tmp/pulsehelper.login 2>&1

    #     # Incantation to enable all four speakers on Carbon X1 Gen 7
    #     # https://gist.github.com/hamidzr/dd81e429dc86f4327ded7a2030e7d7d9
    #     # https://forums.lenovo.com/t5/Ubuntu/Guide-X1-Carbon-7th-Generation-Ubuntu-compatability/m-p/4489823?page=15#5085965
    #     # sudo /usr/bin/hda-verb /dev/snd/hwC0D0 0x17 SET_CONNECT_SEL 1
    # fi

    # End things to do only on tty1

else

    # If we're logged in over a serial port, we might be using screen
    # on something like a Raspberry Pi,
    # in which case we need to set the terminal size explicitly:
    if [[ $(tty) =~ /dev/ttyAMA0 ]]; then
        termsize
    fi
fi

if [[ $(tty) == /dev/pts/0 ]]; then
    echo "====== /dev/pts/0: =====================" >>/tmp/pulsehelper.pts0
    pulsehelper >> /tmp/pulsehelper.pts0 2>&1
    echo "**** pacmd list-sinks:" >> /tmp/pulsehelper.pts0
    pacmd list-sinks >> /tmp/pulsehelper.pts0 2>&1
    echo "**** Setting:" >> /tmp/pulsehelper.pts0
    pulsehelper --source none --sink spkr >> /tmp/pulsehelper.pts0 2>&1
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

if [[ -s ~/.reminders ]]; then
    echo "==================================================="
    cat ~/.reminders
fi

if [[ $(hostname) == 'charon' ]]; then
    echo "==================================================="

    # rancmd() {
    #     echo "Random command of the day:"
    #     ranman=$(ls -1 /usr/share/man/man1/ /usr/share/man/man8 | shuf -n 1)
    #     echo "(from $ranman)"
    #     man -f $(echo $ranman | sed 's_\.[0-9].*__')
    # }
    # rancmd

    if [[ -x ~/bin/reminders && -e ~/web/cal/remind.txt ]]; then
        echo "==================================================="
        ~/bin/reminders week
    fi

    acpi
fi

# Check whether a package tried to overwrite the TTYVTDisallocate setting:
# the file has now been diverted by
# dpkg-divert --divert /lib/systemd/system/getty@.service.new --rename /lib/systemd/system/getty@.service
# Remove the diversion with
# dpkg-divert --remove /lib/systemd/system/getty@.service
if [[ -e /lib/systemd/system/getty@.service.new ]]; then
    echo '*****************'
    echo "Systemd tried to install a new /lib/systemd/system/getty@.service"
    echo "diff /lib/systemd/system/getty@.service /lib/systemd/system/getty@.service.new"
    echo '*****************'
fi

