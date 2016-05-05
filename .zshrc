#! /usr/bin/zsh
#########################
# Akkana's .zshrc
#########################

# User specific aliases and functions

# Get noninteractive shells out of here
if tty -s
then
 :
else
  return
fi

# Source global definitions
if [[ -f /etc/zshrc ]]; then
	. /etc/zshrc
fi

#setopt ignoreeof

setopt RM_STAR_SILENT

# Allow completions like *vol*<tab>
setopt globcomplete

# In a case like rm a/*T.jpg b/*T.jpg, don't fail to remove a/*T.jpg
# just because b/*T.jpg didn't match anything.
# But people on #zsh give dire warnings that this will be somehow unstable.
# unsetopt nomatch
# The only obvious thing wrong with it I've seen is that null matches
# pass a wildcard through, e.g. echo *XX* passes '*XX*'
# which apparently is what bash does.
# 
# This might be a better option. If it matches nothing, it returns nothing.
# setopt nullglob
# 
# Best solution: do what tcsh does!
setopt cshnullglob

# Don't autocomplete non-executable files in PATH (slower but less confusing):
# setopt hashexecutablesonly
# But here's a better solution that lets us know when there's a problem.
# It needs to always return false except in the -e case.
# command_not_found_handler() { echo $? && return 1 };
# This also works but is more verbose:
setopt printexitvalue
# Or this much more elaborate solution:
# . $HOME/.zsh.printexit

# zsh docs say it should be colon separated, but that doesn't work.
# Or sometimes it does -- only on the laptop, for some reason,
# when I'm not reading mail (maybe it's affected by mutt changing
# the atime?) and it's super annoying there.
#mailpath=($HOME/Msgs/in/Inbox $HOME/Msgs/in/whitelist)
#MAIL=(0 $HOME/Msgs/in/Inbox $HOME/Msgs/in/whitelist)

# Prevent any repeated entries in $PATH
typeset -U PATH

# Set path
export PATH=$HOME/bin:$HOME/bin/linux:/usr/local/bin:/usr/local/gimp-git/bin:/usr/sbin:/usr/bin:/bin:/usr/bin/X11:.:/sbin:/usr/games:$HOME/outsrc/android-sdk-linux/platform-tools:$HOME/outsrc/android-sdk-linux/tools:/usr/local/bin:$HOME/.local/bin

export PYTHONPATH=$HOME/bin

ulimit -c unlimited
HISTSIZE=200

# If EDITOR is vim, zsh will try to be "smart" and switch to vi mode.
# This switches bindings back to emacs:
bindkey -e

# Allow pasting functions with comments
# However, this interferes with being able to use # on the commandline.
setopt interactivecomments
# Also, you can't use # on the commandline because it's a special zsh global.
# To use it:
# unsetopt extendedglob

# But really, the only reason I want to use # on the commandline
# is for the stupid xchatlogs. And even aside from the \#, they're
# a pain to complete. Why not make a key binding to pre-type most of it?
# This puts it on F8:
# bindkey -s '\e[20~' '~/.xchat2/xchatlogs/ \\\#^B^B^B'

# This would be better,
# bindkey -s '.xx' '~/.xchat2/xchatlogs/ \\\#^B^B^B'
# but you can't type it interactively because
# you end up with recursion and it inserts
# ~/~/.xchat2/xchatlogs/hat2/xchatlogs/ \# \#

# Here's an even better way that doesn't require using a function key:
# it puts it on .xc (when typed quickly).
# The LBUFFER/RBUFFER stuff are to avoid recursion:
# they modify the strings before and after the cursor.
# See http://zsh.sourceforge.net/Guide/zshguide04.html
# under 4.7.4: Special parameters: normal text
# Or http://stackoverflow.com/questions/6673280/avoid-recursion-in-zsh-command-line
function autoxchat()
{
    LBUFFER+="~/.xchat2/xchatlogs/"
    RBUFFER=" \\#$RBUFFER"
}
zle -N autoxchat
bindkey "=xc" autoxchat

# Prompt setting

# Linux consoles don't colorize bold, so make it magenta to stand out
# against the black background:
# rxvt lets me set bold to be a different color that contrasts better
# than any of the ANSI colors. So use that if possible:
if [[ $TERM == 'rxvt' ]]; then
  export standout="\e[1;m"

# On a linux console, use magenta because it contrasts with the black bg:
elif [[ $TERM == 'linux' ]]; then
  export standout="\e[1;35m"

# All others, use blue:
else
  export standout="\e[0;34m"
fi
export standout_end="\e[m"

# Only set this prompt if I'm logged in as myself:
if [[ $USER == akkana ]]; then
  # PS1=$'%{\e[1m%}<'$(hostname)$primes$'>-%{\e[0m%} '
  #PS1='%K{white}%F{blue}<'$(hostname)$primes$'>- %f%k'
  PS1='%F{blue}<'$(hostname)$primes$'>- %f%k'
elif [[ $USER == root ]]; then
  #PS1=$'%{\e[1m%}#['$(hostname)$primes$']#%{\e[0m%} '
  PS1='%K{white}%F{red}['$(hostname)$primes$'#]- %f%k'
fi
export primes=${primes}\'

# Print useful info on the right.
# %F{color} sets the color; %f%k restores default fg/bg colors, respectively.
# %t is time in 12-hr format (%T 24);
# %~ is current directory;
#RPS1='%~%w %t'
# RPS1=" %F{red}%~ %t%f%k"
# Just current dir, no time:
RPS1="%F{red}%~%f%k"
# Cool happy/sad face right prompt from saz, from a friend of hers,
# changes with status of the last command:
# RPS1=''%(?,"$(print '%{\e[1;35m%}:-)%{\e[0m%}')","$(print '%{\e[1;31m%}:-(%{\e[0m%}')")''

# Bash defaults to a really short timeout, and exits on inactivity.
# Not sure if zsh needs this as well.
TMOUT=0

# Environment
export PAGER=less
# Need -er in LESS, for git colors to work
export LESS="-EerX"
export LC_COLLATE=C

export EDITOR=vim
# Be sure to set bindkey -e -- done above with the other bindkey stuff.

# See http://www.linux-sxs.org/housekeeping/lscolors.html
export LS_COLORS='ex=1;31:ln=1;35'

export RSYNC_RSH=ssh
export PHO_ARGS=-p

#
# Aliases and functions.
#

alias m=mutt

show_symlinks() {
    for f in $*; do
        # Remove terminal slash.
        f=${f%/}
        # Mikachu: if you have extendedglob set you can use %{f%%/#}
        # to remove all trailing slashes
        #f=`echo $f | sed 's/\/$//'`
        # zsh is supposed to be able to do this with globbing,
        # bug in practice no one anywhere seems to have any working
        # examples of the glob qualifiers like #e.
        # Better method, from http://www.zzapper.co.uk/zshtips.html
        # But #e only works with extendedglob.
        #setopt extendedglob
        # man zshall talks about EXTENDED_GLOB but zsh options are
        # case insensitive and ignore underscores, and convention seems
        # to use lowercase and omit the underscores.
        #f=${f/\/#e/}
        if [[ -h $f ]]; then
            line=( $(/bin/ls -ld $f ) )
            #echo Symlink: $line[-3,-1]
            echo -E Symlink: $line[9,-1]
        #else
        #    echo $f is not a symlink
        fi
    done
}
ls() { /bin/ls -FH $* ; }
ll() {
    /bin/ls -laFH $*
    show_symlinks $*
}
llt() { /bin/ls -laSHFLt $* ; }
llth() { /bin/ls -lFSHLt $* | head -20 ; }

# There are lots of ways to list only directories. Here are some:
#lsdirs1() { env ls -1FH "$1" | sed -n 's|/$||p' | column; }
#
#lsdirs2() {
#  (cd $1; /bin/ls -d `/bin/ls -1F | grep / | sed 's_/$__'`)
#}

lsdirs() { 
  echo `/bin/ls -1F $@ | grep / | sed 's_/$__'`| tr -s ' ' '\n' | paste - - - | column -x -t -c3
}

alias j=jobs
alias pd=pushd
alias s=suspend
alias rl="telnet -r"

alias beep="echo "
alias akk="aplay $HOME/.xchat2/sounds/akk.wav"
alias ap="man -k"

alias screenshot="scrot -b -s screenshot.jpg"
alias thes="dict -h localhost -d moby-thesaurus"

alias netscheme='sudo /home/akkana/src/netutils/netscheme'

# Newer versions of xterm no longer support titlebar setting with
# the documented sequence of \e]2. But \e]0 works, as long as you
# don't set XTerm*allowSendEvents.
titlebar() {
  # echo ']]2;$*'
  echo -e "\033]0; $* \007"
}

# Spelling check
sp() {
  spell $* | sort | uniq
}

###############################
# Recursive greps
gr() {
  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pyc' -and -not -name '*.jpg' -and -not -name '*.JPG' -and -not -name '*.png' -and -not -name '*.xcf*' -and -not -name 'po' -and -not -name '*.tar*' -and -not -name '*.zip' -or -name '.metadata' -prune \) -print0 | xargs -0 grep $* /dev/null | fgrep -v .svn | fgrep -v .git
}
zgr() {
  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pyc' -and -not -name '*.jpg' -and -not -name '*.JPG' -and -not -name '*.png' -and -not -name '*.xcf*' -and -not -name 'po' -and -not -name '*.tar*' -and -not -name '*.zip' -or -name '.metadata' -prune \) -print0 | xargs -0 zgrep $* /dev/null | fgrep -v .svn | fgrep -v .git
}
cgr() {
  find . \( -name '*.[CchH]' -or -name '*.cpp' -or -name '*.cc' \) -print0 | xargs -0 grep $* /dev/null
}
hgr() {
  find . \( -name '*.h' -or -name '*.idl' \) -print0 | xargs -0 grep $* /dev/null
}
rgr() {
  find . \( -name '*.rb' -or -name '*.rhtml' \) -print0 | xargs -0 grep $* /dev/null | fgrep -v .svn
}
htgr() {
  find . -name '*.*htm*' -print0 | xargs -0 grep $* /dev/null
}
jgr() {
  find . -name '*.js' -print0 | xargs -0 grep $* /dev/null
}
xgr() {
  find . \( -name '*.cChH' -or -name '*.cpp' -or -name '*.xul' -or -name '*.html' -or -name '*.js' -or -name '*.css' \) -print0 | xargs -0 grep $* /dev/null
}
cssgr() {
  find . -name '*.css' -print0 | xargs -0 grep $* /dev/null
}
mgr() {
  find . -name '*akefile*' -print0 | xargs -0 grep $* /dev/null
}
agr() {
  find . -type f -print0 | xargs -0 grep $* /dev/null
}
javagr() {
  find . -name '*.java' -print0 | xargs -0 grep $* /dev/null
}
# zgr() {
#  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 zgrep $* /dev/null
#}
# Next doesn't work. How do we use -prune?
idagr() {
  find . \( -name OBJ -prune -or -name external -prune -or -name '*scons*' -prune -or -name google_appengine -prune -o -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pyc' \) -print0 | xargs -0 grep $* /dev/null | fgrep -v .svn | fgrep -v .git
}

alias pygrep="langgrep python"

# Grep stdin for lines that have any of these terms.
# usage: cmd | grepany term1 term2 term3 
grepany() {
    egrep "(${(j:|:)*})"
}

# Grep stdin for lines that have all of these terms
# (and none of the terms after the -v).
# usage: cmd | grepall term1 term2 term3 -v term4 term5
grepall() {
    vterm=${*[(i)-v]}
    pos=($*[1,$vterm-1])
    neg=($*[$vterm+1,-1])

    cmd="cat"
    for term in $pos; do
        cmd="$cmd | grep $term"
    done
    for term in $neg; do
        cmd="$cmd | grep -v $term"
    done

    zsh -c $cmd
}

# pushd, but not if we're already at the target directory
# or if we're currently home.
# Use in other scripts that need to save the previous directory.
pushd_maybe() {
    cwd=`pwd`
    if [[ x$1 == x$cwd ]]; then
        return
    fi
    if [[ x$cwd == x$HOME ]]; then
        cd $1
    else
        pushd $1
    fi
}

popd_maybe() {
    # $dirstack isn't documented anywhere near pushd/popd/dirs,
    # but it works. Apparently it's documented with the zsh/parameters
    # module in zshmodules(1).
    if [[ $#dirstack > 0 ]]; then
        popd
    fi
}

# end greps
#######################################################

# Update all my git repositories:
allgit() {
    pushd_maybe ~
    foreach repo ( scripts netutils metapho feedme imagebatch pytopo
                   gimp-plugins pho htmlpreso android dotfiles arduino )
        echo $repo :
        cd ~/src/$repo
        git pull --all
    end
    popd_maybe
}

# If you're working on a branch, and all your changes are committed,
# use this to merge master changes into the current branch.
alias git_merge_branch='git fetch; git rebase origin/master'

# Run a git with all-default settings.
# Usage: gimpclean VERSION SWM
# e.g. gimpclean git no, gimpclean 2.8 yes
gimpclean() {
    gimpdir=`mktemp -d /tmp/gimpenv.XXXXX`
    if [[ x$1 != x ]]; then
        version=-$1
    else
        version=
    fi
    if [[ x$2 == 'xno' ]]; then
        echo swm no
        # This doesn't really work, alas.
        echo "(single-window-mode no)" > $gimpdir/sessionrc
    fi
    echo version is $version
    GIMP2_DIRECTORY=$gimpdir gimp$version --new-instance
}

# Don't accidentally halt on server machines.
hostname=$(hostname)
if [[ $hostname == 'moon' || $hostname == 'dna' ]]; then
  alias off="echo This is $hostname, you fool!"
  alias halt="echo This is $hostname, you fool!"
  alias reboot="echo This is $hostname, you fool!"
else
  alias off="sudo shutdown -h now"
  alias halt="sudo shutdown -h now"
  alias reboot="sudo shutdown -r now"
fi

######## audio/video aliases

# mencoder options are black magic.
# This works for converting Minolta quicktime .mov to mpeg:
mov2mpg1() {
  mencoder $1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg1video -o $2
}
# -lavc is ffmpeg, and the default codec is divx:
mov2divx() {
  mencoder $1 -oac pcm -ovc lavc -o $2
}
# From drc on #gimp:
mov2mpeg4() {
  mencoder $1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg4:vqmin=2:vlelim=-4:vcelim=9:lumi_mask=0.05:dark_mask=0.01:vhq -o $2
}

# Extract audio from flash:
# mov2mp3old() {
#   avconv -i $1 $2
# }
#
# But that doesn't work any more (2015) and takes forever, so try this instead:
alias tomp3='soundconverter -b -m "audio/mpeg" -s ".mp3"'

# Record a realaudio stream
getreal() {
  mplayer -playlist $1 -ao pcm:file=$2 -vc dummy -vo null
}
# Then transcode it with: 
# lame --tg Other --ta artist -tl album file.wav file.mp3

######## end video aliases

# Use LibreOffice to convert doc to html:
LOdoc2html() {
    libreoffice --headless --convert-to html:HTML --outdir doc2html $1
}

# Extract text out of a ppt:
# http://superuser.com/questions/661315/tools-to-extract-text-from-powerpoint-pptx-in-linux
ppt2txt() {
    unzip -qc "$1" ppt/slides/slide*.xml | grep -oP '(?<=\<a:t\>).*?(?=\</a:t\>)'
}

# Remove the line matching $1 from ~/.ssh/known_hosts.
# Ssh refuses to operate if anything has changed about the host:
# network card, distro it's running, etc.
cleanssh() {
  mv $HOME/.ssh/known_hosts $HOME/.ssh/known_hosts.bak
  egrep -v "^\[?$h\]?\b" $HOME/.ssh/known_hosts.bak >$HOME/.ssh/known_hosts
}

# Get the temperature from /proc/acpi/thermal_zone/THRM/temperature
# and convert it to F
#alias temp="cat /proc/acpi/thermal_zone/*/temperature"
alias temp=sensors

# For presentations
# alias bigterm="rxvt -geometry 80x33 -fn '-*-lucidatypewriter-*-*-*-*-19-*-*-*-*-*-*-*'"
alias bigterm="rxvt -fn terminus-iso8859-2-bold-18"
alias noteterm="xterm -geometry 30x34+1025+0 -fn '-*-terminus-bold-*-*-*-22-*-*-*-*-*-*-*'"
# For notes during planetarium shows:
# red/black for night vision, narrow to show two at once on a laptop.
alias planeterm="rxvt -geometry 62x45 -fn terminus-iso8859-2-bold-18 -bg black -fg red"

#alias zzz='sudo /etc/acpi/sleep.sh'
alias zzz='sudo pm-suspend --auto-quirks'

########## Fiddle with external monitor/audio connections:
# Connect to a projector on the VGA port:
alias projector='xrandr --output VGA1 --mode 1024x768'
# and on the HDMI port:
alias projectorh='xrandr --output HDMI1 --mode 1024x768'

# and set video back to normal:
alias monitor='xrandr --output HDMI1 --mode 1680x1050 --output VGA1 --off --output LVDS1 --off'
alias noprojector='xrandr --auto'

# Send all audio output to HDMI.
# Usage: hdmisound [on|off], default is on.
hdmisound() {
    if [[ $1 == 'off' ]]; then
        if [[ -f ~/.asoundrc ]]; then
            mv ~/.asoundrc ~/.asoundrc.hdmi
        fi
        amixer sset IEC958 mmute
    else
        if [[ -f ~/.asoundrc ]]; then
            mv ~/.asoundrc ~/.asoundrc.nohdmi
        fi
        cat >> ~/.asoundrc <<EOF
pcm.dmixer {
  type dmix
  ipc_key 1024
  ipc_key_add_uid false
  ipc_perm 0660
  slave {
    pcm "hw:0,3"
    rate 48000
    channels 2
    period_time 0
    period_size 1024
    buffer_time 0
    buffer_size 4096
  }
}
 
pcm. !default {
  type plug
  slave.pcm "dmixer"
}
EOF
        amixer sset IEC958 unmute
    fi
}
########## End external monitor/audio connections:

# Mount encrypted SD card:
cryptmount() {
    device=$1
    name=$2
    sudo cryptsetup luksOpen $device $name
    sudo mount /dev/mapper/$name /$name -o defaults,relatime
}
cryptunmount() {
    name=$1
    sudo umount /$name
    sudo cryptsetup remove $name
}
if [[ $HOST == 'vaiolin' ]]; then
  alias crypt='cryptmount /dev/mmcblk0p2 crypt'
#elif [[ $HOST == 'imbrium' ]]; then
#  # Should base this on whether /dev/sd[b-e] already exist
#  alias crypt='cryptmount /dev/sdf3 crypt'
else
  #alias crypt='cryptmount /dev/sdc3 crypt'
  alias crypt='cryptmount /dev/disk/by-uuid/170f3caa-412f-41b7-90a8-1c1b149cec8c crypt'
fi
alias uncrypt='cryptunmount crypt'

# Serial connections to plug computers:
#alias plug='minicom -D /dev/ttyUSB1 -b 115200'
alias plug='screen /dev/ttyUSB1 115200'
alias guru='screen /dev/ttyUSB0 115200'
alias rpi='titlebar "Raspberry Pi"; screen /dev/ttyUSB0 115200; titlebar "local"'
alias pion='titlebar "Raspberry Pi Pion"; ssh -X pi@pion; titlebar "local"'

# Connect/disconnect from a docking station. Obsoleted by shell script.
#alias dock='xrandr --output VGA1 --mode 1600x900; hsetroot -center `find -L $HOME/Backgrounds -name "*.*" | randomline`; xrandr --output LVDS1 --off'
#alias undock='xrandr --output LVDS1 --mode 1280x800; hsetroot -center `find -L $HOME/Backgrounds -name "*.*" | randomline`'

####################################################
# zsh-specific options:

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory notify
unsetopt autocd
# End of lines configured by zsh-newuser-install

# This is apparently Ubuntu-specific weirdness:
#skip_global_compinit=1

# The following lines were added by compinstall
#zstyle :compinstall filename '/home/akkana/.zshrc'

# End of lines added by compinstall

# When typing a |, don't eat the space before it.
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'

# Older zsh, like on squeeze, don't have compdef.

# zsh annoyingly only prints the last 10 lines of history by default.
alias history='history 200'

###################################################
######## zsh completion stuff #####################

# Place to add custom completion scripts
fpath=(~/.config/zsh/completion $fpath)

# When feeling that zsh completion is just too annoying and too buggy,
# you can turn it off by commenting out these two lines:
autoload -Uz compinit
compinit

# Don't autofill the first match of a list of ambiguous matches:
setopt noautomenu

# When showing menu help, include descriptions:
zstyle ":completion:*:descriptions" format "%B%d%b"

# Much more verbose info (while learning/debugging compdefs).
# But these don't actually work, no matches found: ‘:completion:*’
zstyle ":completion:*" verbose yes
zstyle ":completion:*:descriptions" format '%B%d%b'
zstyle ":completion:*:messages" format '%d'
zstyle ":completion:*:warnings" format 'No matches for: %d'
zstyle ":completion:*" group-name

# Some tuts on writing custom completions:
# http://askql.wordpress.com/2011/01/11/zsh-writing-own-completion/
# http://www.linux-mag.com/id/1106/

WORDCHARS=$WORDCHARS:s,/,,
# See also http://mika.l3ib.org/s/dot-delete-to

#
# Slash removal:
#
# If annoyed by tab-completion including slashes too much, try this:
# Mika: the slash thing is ZLE_REMOVE_SUFFIX_CHARS and ZLE_SPACE_SUFFIX_CHARS

# Or this, which is supposed to prevent removing the slashes -- but that means
# it still leaves them in place on symlinks to directories.
# And in any case it doesn't work: ls /tmp<tab> adds a slash,
# then typing a space makes the slash disappear, even with this.
unsetopt AUTO_REMOVE_SLASH

# But doing that messes up autocompletion on symlinks:
# zsh addsj / at the end of autocompleted symlinks to directories
# (e.g. mv path/to/symlink<TAB> path/to/otherplace files with
# "Not a directory") but it doesn't help.

# Mikachu's clever hack to avoid having the slashes disappear
# when I type a line like rsync -av dir/ /back/dir/
# I'm not sure this is actually any better than unsetopt AUTO_REMOVE_SLASH, tho.
# <Mikachu> the . means to run the builtin widget, not whatever function is overloading it
# <Mikachu> so without it, it would just recurse forever
# Dana notes that you can also type an extra slash
# to make the autocompleted slashes stay there
# (but you can't tell visually whether a slash is "real" or not).
#
# function accept-line() {
#   zle auto-suffix-retain
#   zle .$WIDGET
# }
# zle -N accept-line
# ZLE_REMOVE_SUFFIX_CHARS=
#
# The solution is probably to set up a special completion for rm
# that checks whether its argument is a directory.
# Start with /usr/share/zsh/functions/Completion/Unix/_rm
# also http://zsh.sourceforge.net/Doc/Release/Completion-System.html

# /usr/share/zsh/functions/Completion/Unix/_hosts autocompletes hosts
# case-insensitively, which means that any rule (like rsync) that
# uses hosts becomes case-insensitive if there's a hostname that
# might match. Ideally I should just remove that and make it complete
# case-sensitively; but until I learn how to do that, just turn off
# hostname completion entirely, since I don't actually use it:
#_hosts() { }

# Turning off completions that are too smart for their own good:
if [ -n "$_comps" ]; then
  # zsh has some kind of "smart" git completion that doesn't autocomplete
  # file or directory names. I ask you, how smart is that?
  # But blah! compdef doesn't exist in the zsh in Debian squeeze.
  # I hope they don't have the smart completion either.
  #compdef _files git

  # By default (no CLASSPATH SET), autocompletion for java searches
  # recursively starting from .  Don't try it in your homedir!
  # Not sure if this really turns it off, though -- had a typo.
  #compdef _files java

  # loadkeys also has "smart" (* un-smart) completion.
  compdef _files loadkeys

  # adb hangs trying to autocomplete anything -- apparently it's
  # actually trying to talk to the android even when you're trying
  # to autocomplete a local filename.
  compdef _files adb

  # Other things that have broken autocomplete, so tell it to just
  # look for filenames like a normal well-behaved shell:
  # Actually unrar completion may not be broken after all, wait and see.
  #compdef _files unrar
fi

#
# Autocompletion related key bindings:
#

# bind "menu behavior" (i.e. complete to the first match, then to
# successive matches upon repeat use) to another key:
# Normally this is on \t\t, which is super annoying, so unbind that:
bindkey -r '\t\t'
bindkey '\e\t' menu-complete

# If you need to know what rules zsh is using for a completion.
# This only works if you've run compinit.
#bindkey '\e\d' _complete_help
bindkey '\e\e' _complete_help

# zsh sometimes replaces a command with the completions when you hit tab.
# But you can undo that and go back to what you were typing.
# Normally it's ^X^U but this is easier to remember:
bindkey '^Z' undo
bindkey '^X^R' redo

# Two other useful bindings suggested by Mikachu:
# the latter lets you press a key and see what's bound to it,
# the former finds all keys that are bound to the specified widget
# since you can also tabcomplete, you can also usually find
# something useful by typing a prefix and tabbing
bindkey '^X^W' where-is
bindkey '^X^D' describe-key-briefly

######## end zsh completion #######################
######## end zsh-specific options #################

# Text to speech:
# From commandlinefu:
#say() { mplayer "http://translate.google.com/translate_tts?q=$1"; }
# That no longer works (maybe google is refusing connections that
# don't have a known browser string),
# but this does, thanks Carla!
# http://www.linux.com/learn/docs/660651-bag-of-fun-and-useful-random-linux-comman
emplussen() {
    str=$(echo $* | sed 's/ /\+/g')
    echo $str
}

say() {
    str=$(emplussen $*)
    wget -q -O- -U Mozilla "http://translate.google.com/translate_tts?q=$str&tl=en-us" |cvlc - |play -t wav - -t wav -t alsa
}

sayuk() {
    str=$(emplussen $*)
    wget -q -O- -U Mozilla "http://translate.google.com/translate_tts?q=$str&tl=en-uk" |cvlc - |play -t wav - -t wav -t alsa
}

#############################################################
# Android aliases

# Some aliases for getting files from Android KitKat via adb,
# since the lack of usb-storage and autocomplete is such a pain.

# There seems to be no way to remove multiple or wildcarded files via adb.
# alias delallgpx='adb shell rm /mnt/extSdCard/Android/data/net.osmand.plus/tracks/rec/*'

pullgpx() {
  pushd_maybe ~/Docs/gps/new
  adb pull /storage/extSdCard/Android/data/net.osmand.plus/files/tracks/rec/. .
  for f in *.gpx; do
    echo $f
    adb shell rm /storage/extSdCard/Android/data/net.osmand.plus/files/tracks/rec/$f
  done
  ls
}

pullphotos() {
  pushd_maybe ~/Docs/gps/new
  adb pull /storage/extSdCard/DCIM/Camera/. .
  for f in *.jpg *.mp4; do
    echo $f
    adb shell rm /storage/extSdCard/DCIM/Camera/$f
  done
  ls
}

# But what if we don't have adb installed? Here's how to do it using gphoto2.
alias pullphotosg='gphoto2 --folder /store_00020002/DCIM/Camera -P'
alias delphotosg='gphoto2 --folder /store_00020002/DCIM/Camera -D'

# And similar aliases for gpx:
alias pullgpxg='gphoto2 --folder /store_00020002/Android/data/net.osmand.plus/files/tracks/rec -P'
alias delgpxg='gphoto2 --folder /store_00020002/Android/data/net.osmand.plus/files/tracks/rec -D'

# Android adb logcat is supposed to accept a filter argument to show only
# logs from a single program, but it doesn't work and I have to use grep.
# But just grepping for the program name gets tons of extra lines from
# every tap or other event delivered to the program;
# and piping grep to grep makes grep buffer its output
# unless the --line-buffered flag is specified.
# --line-buffered probably isn't needed on the last grep.
adebug2() {
    adb logcat | egrep --line-buffered "($1|E/AndroidRuntime)" | grep -v --line-buffered Delivering
}
adebug() {
    adb logcat -s ActivityManager:I AndroidRuntime:E $1:D '*:S'
}
alias debugfeed='adebug FeedViewer'
# alias df2='adb logcat | egrep --line-buffered "(Feed|E/AndroidRuntime)" | grep -v --line-buffered Delivering'

# For building Android apps such as osmand:
export ANDROID_HOME=$HOME/outsrc/android-sdk-linux
export ANDROID_SDK=$HOME/outsrc/android-sdk-linux
export ANDROID_NDK=$HOME/outsrc/android-ndk-r10d

# Find location of Android imports:
andimport() {
    find $ANDROID_SDK -name $1.java
}

# https://code.google.com/p/osmand/wiki/GradleCommandLineBuildEnvironment
alias osmandbuild='cd ~/outsrc/osmand/android; repo sync -d; cd OsmAnd; ../gradlew --refresh-dependencies clean assembleFullLegacyFatDebug'

phof () {
    pho `fotogr $*`
}

# Copy the primary selection into the clipboard:
alias primary2clip='xsel -p | xsel -i -b'
# and vice versa:
alias clip2primary='xsel -b | xsel -i -p'

# Tail the procmail log file, for when I'm expecting mail:
alias proctail="tail -f Procmail/log | egrep -v '^procmail'"

# What's the complement of a number, e.g. the fmask in fstab to get
# a given file mode for vfat files? Sample usage: invert 755
invertmask() {
    python -c "print '0%o' % (0777 - 0$1)"
    # This also works:
    # python -c "print '0%o' % (~(0777 & 0$1) & 0777)"
}

alias s4='jmtpfs /s4'
# Unmount with fusermount -u /s4

# Clean up libreoffice HTML conversions:
# tidy -q -config ~/tidy_options.conf -i /tmp/LWVNM\ Convention\ 2015\ Minutes.html | sed -e 's/ class="[cP][0-9]*"//g' -e 's/ class="[cP][0-9]* [cP][0-9]*"//g' > /tmp/tidied.html

################################################
# Various GPS conversions

kml2gpx() {
    # :t takes the basename, :r removes the extension
    gpsbabel -i kml -f $1 -o gpx -F $1:t:r.gpx
}

kmz2gpx() {
    # unzip the KMZ, since gpsbabel STILL doesn't know how to do that
    # despite KMZ becoming the most popular format on the net:
    kmlfile=/tmp/$1:t:r.kml
    gunzip -c $1 > $kmlfile
    # :t takes the basename, :r removes the extension
    gpsbabel -i kml -f $kmlfile -o gpx -F $kmlfile:t:r.gpx
}

# Convert a pair of UTM coordinates in NM to a GPX file with one waypoint.
# I don't know how to get the UTM zone if you don't already have it;
# Barbara just gives me the pair of points without a zone.
utm2gpx() {
    unicsv=`mktemp /tmp/point-XXXXX.csv`
    gpxfile=$unicsv:r.gpx
    echo "name,utm_z,utm_e,utm_n,comment" >>$unicsv
    printf "Point,13,%s,%s,point" $1 $2 >>$unicsv
    gpsbabel -i unicsv -f $unicsv -o gpx -F $gpxfile
    echo Created $gpxfile
}

# Converting back is harder, but gpsbabel's "text" format gives both:
gpx2utm() {
    gpsbabel -i gpx -f $1 -o text -F -
}
# End GPS conversions
################################################

################################################
# Build/development helpers

# I get tired of all the multiple steps to update gimp now that it
# requires three separate repositories.
gimpmaster() {
    # Make sure this exits on errors!
    setopt localoptions errreturn

    pushd_maybe ~/outsrc/libmypaint
    # Don't do this every time. But if we did, this is what to do:
    # scons prefix=/usr/local/gimp-git/ enable_gegl=true
    # sudo scons prefix=/usr/local/gimp-git/ enable_gegl=true install

    cd ~/outsrc/babl
    git pull
    make -j4
    sudo make install

    cd ~/outsrc/gegl
    git pull
    make -j4
    sudo make install

    # Hopefully we don't have to update mypaint every time.
    # cd ~/outsrc/libmypaint
    # scons prefix=/usr/local/gimp-git enable_gegl=true
    # sudo scons prefix=/usr/local/gimp-git enable_gegl=true install 

    cd ~/outsrc/gimp
    git pull
    make -j4
    sudo make install
    popd_maybe
}

# It often happens that some change in the build system makes autogen/configure
# fail, which also prevents you from doing a make clean.
# But sometimes, running autogen.sh with no arguments will fix this,
# let you run a distclean, and then everything will work again.
alias distclean1="./autogen.sh && ./configure && make clean"

distclean() {
    setopt localoptions errreturn

    args=$(egrep '^  \$ ./configure' config.log | sed 's_^  \$ ./configure __')
    echo "Saving args:" $args
    ./autogen.sh
    ./configure
    make clean

    echo "=========================================="
    echo "Running ./autogen.sh $args"
    sleep 3
    ./autogen.sh $args
}

# Make an android tar file from the arg or current directory:
# just the source files without all the eclipse workspace crap.
droidtar() {
    name=$1
    if [[ $name == '' ]]; then
        name=$(basename $PWD)
        cd ..
    fi
    # Remove terminal slash.
    name=${name/\//}
    date=$(date +%Y-%m-%d)
    echo "Making tar file of $name on $date"
    tarfile="$name-$date.tar.gz"
    tar czvf $tarfile $name/AndroidManifest.xml $name/default.properties $name/src $name/res
    echo "Created $tarfile"
}

newhexchat() {
    # Can't set errreturn yet, because that will cause mv and rm
    # (even with -f) to exit if there's nothing to remove.
    cd ~/outsrc/hexchat-debian
    echo "Removing what was in old previously"
    rm -rf old
    echo "Moving everything here to old/"
    mkdir old
    mv *.* old/

    # Make sure this exits on errors from here on!
    setopt localoptions errreturn

    echo "Getting source ..."
    apt-get source hexchat
    cd hexchat-2*
    echo "Patching ..."
    patch -p0 < ~/outsrc/hexchat-2.10.2.patch
    echo "Building ..."
    debuild -b -uc -us
    echo
    echo 'Installing' ../hexchat{,-python,-perl}_2*.deb
    sudo dpkg -i ../hexchat{,-python,-perl}_2*.deb
}

# Check on status of all held packages:
check_holds() {
    for pkg in $( aptitude search '~ahold' | awk '{print $2}' ); do
        policy=$(apt-cache policy $pkg)
        installed=$(echo $policy | grep Installed: | awk '{print $2}' )
        candidate=$(echo $policy | grep Candidate: | awk '{print $2}' )
        if [[ "$installed" == "$candidate" ]]; then
            echo $pkg : nothing new
        else
            echo $pkg : new version $candidate available
        fi
    done
}

# End build/development helpers
################################################

# Some handy battery scripts from d:

bat() {
  #cat /proc/acpi/battery/BAT1/state
  #cat /proc/acpi/battery/BAT1/info
  acpitool -B | egrep "^ *[CPR][ehr]"
}

batt() {
  #cat /proc/acpi/battery/BAT1/state
  #cat /proc/acpi/battery/BAT1/info
  acpitool -B
}

volts() {
  acpi -i
  calc `cat /sys/class/power_supply/BAT0/voltage_now` / 1000000
  acpitool -B | grep Present
}

# Playing DVDs with mplayer. f => fullscreen, v -> no subtitles
alias playdvd="mplayer dvd://1 -alang en"

# Making a PDF from a bunch of slides
alias talk2pdf='qhtmlprint $( fgrep .html navigate.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '
alias talk2pdf1024='qhtmlprint -1024 $( fgrep .html navigate.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '
alias talk2pdf1366='qhtmlprint -1366 $( fgrep .html navigate.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '

# Reduce the size of a PDF. Usage: pdfreduce infile.pdf outfile.pdf
# http://ubuntuforums.org/showthread.php?t=1133357
# You can also replace /screen with /ebook for slightly higher image quality.
# This may cause vector diagrams to be removed, so be sure to check
# before vs. after.
# Can experiment with removing -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen
# or just -dPDFSETTINGS=/screen.
pdfreduce() {
    gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dNOPAUSE -dQUIET -dBATCH -sOutputFile="$2" $1
}

# Which printers are available? lpstat -p -d also works.
alias whichprinters='lpstat -a'

# lp inconsistently decides to use zero margins. When it does, this helps.
# (In theory, adding -o page-top=17 should add a top margin, but in
# 2014 this seems to make a negative margin, dropping the first few
# lines. All hail Linux printing!)
alias lpp='lp -o page-left=38'

# Mirror a website on a directory. Be sure to include an end slash
# on the URL!
mirror() {
    d=$1
    if [[ $d =~ '.*/$' ]]; then
        wget -np -r $d
    else
        echo "$d needs to end with a slash"
    fi
}

# What's the current time in UT / GMT?
ut() {
    date -u $*
}

# Convert a fixed date (e.g. for a meeting) from UT/GMT.
# date -d 'Tue November 12 18:00 UTC' or date -d '18:00 UTC next Friday'
fromut() {
    date -d $*
}

# Subtract dates
datediff() {
    d1=$(date -d "$1" +%s)
    d2=$(date -d "$2" +%s)
    echo $(( (d1 - d2) / 86400 )) days
    echo $(( (d1 - d2) / 86400 / 7. )) weeks
    echo $(( (d1 - d2) / 86400 / 7 )) weeks $(( (d1 - d2) / 86400 % 7 )) days
}

# Tell aptitude not to limit descriptions to the terminal width
alias aptitude='/usr/bin/aptitude --disable-columns'

# Adjust for day or nighttime monitor modes
alias day="xrandr --output HDMI1 --brightness 1.0"
alias night="xrandr --output HDMI1 --brightness .8"

alias kindle="wine ~/.wine/drive_c/Program\ Files/Amazon/Kindle/Kindle.exe"
alias adobeDE="wine ~/.wine/drive_c/Program\ Files/Adobe/Adobe\ Digital\ Editions/digitaleditions.exe"

# R has no way to tell it not to prompt annoyingly to save the environment
# every time you quit, except as a commandline flag:
alias R="/usr/bin/R --no-save"

# Convert temperatures between F and C, because units' stupid syntax
# is impossible to remember. (If ctemp isn't installed.)
c2f() {
    units "tempC($1)" tempF
}
f2c() {
    units "tempF($1)" tempC
}

##################################
# Spam-related aliases

# Spast checks spam with e.g. echo $subj | grep -i -f $patfile
# How do we find out from $subj which line in $patfile matched the grep?
# Sample Usage: whichspam 'subject-line' subject
whichspam() {
  # to print each line before executing, for debugging purposes:
  #set -o xtrace
  whichfile=$2
  if [[ x$whichfile == x ]]; then
    whichfile=subjectRejects
  fi
  echo Searching in ~/Procmail/spast/$whichfile
  cat ~/Procmail/spast/$whichfile | while read line ; do
    #echo echo "$1" '| egrep -i --' "$line" '>/dev/null'
    #echo "$1" | egrep -i -- "$line" >/dev/null
    # echo "$1" PIPE egrep -i -- "$line"
    echo "$1" | egrep -i -- "$line"
    if [[ $? == 0 ]]; then
      echo $line
    fi
  done
  #set +o xtrace
}

#
# Search for spam subjects or from lines in Spam/saved,
# for purposes of telling which patterns should be added to procmail filters.
#
spams() {
    #grep Subject ~/Spam/saved ~/Spam/trained/saved | egrep -i "$*"
    echo "============ Recent =============="
    decodemail -a Subject: ~/Spam/saved | egrep -a -i "$*"
    echo
    echo "============ Older =============="
    decodemail -a Subject: ~/Spam/oldheaders/saved | egrep -a -i "$*"
}
spamf() {
    #grep -a -h '^From:' ~/Spam/trained/saved ~/Spam/saved | egrep -a -i "$*"
    echo "============ Recent =============="
    decodemail -a From: ~/Spam/saved | egrep -a -i "$*"
    echo
    echo "============ Older =============="
    decodemail -a From: ~/Spam/oldheaders/saved | egrep -a -i "$*"
}
spamff() {
    #grep -a -h '^From' ~/Spam/trained/saved ~/Spam/saved | egrep -a -i "$*"
    decodemail -a From ~/Spam/saved ~/Spam/oldheaders/saved | egrep -a -i "$*"
}

cleanspam() {
    # Spam is saved in ~/Spam. (Outside my regular mail hierarchy,
    # so it doesn't get synced to my laptop or backed up in minibackups.)
    # Older batches have From, Subject, To headers saved in ~/Spam/oldheaders.
    # Periodically, we need to clean out the current spam folders
    # but save the old headers for spam filter development purposes.
    for folder in $HOME/Spam/*; do
        if [[ -f $folder && -s $folder ]]; then
            echo $folder
            # cat $folder >> $HOME/Spam/trained/$(basename $folder)
            decodemail -a 'From:|Subject:|To:' $folder \
                       >> $HOME/Spam/oldheaders/$(basename $folder)
            # Zero the folder out rather than removing it,
            # so mutt won't pester about creating the folder
            # next time we save spam to it.
            cp /dev/null $folder
        fi
    done
    tail -7000 $HOME/Procmail/log >$HOME/Procmail/olog
    rm $HOME/Procmail/log
}
# End spam-related aliases
##################################

# Linux has a lovely list of all compose key sequences.
composekey() {
  grep $1 /usr/share/X11/locale/en_US.UTF-8/Compose
}

alias remindme='remind -g ~/Docs/Lists/remind'

# Display a text calendar some number of months (default 2)
# using my remind database:
mycal() {
    months=$1
    if [[ x$months == x ]]; then
        months=1
    fi
    remind -c$months ~/Docs/Lists/remind
}

# Display a postscript calendar some number of months (default 2)
# using my remind database:
mycalp() {
    months=$1
    if [[ x$months == x ]]; then
        months=2
    fi
    remind -p$months ~/Docs/Lists/remind  | rem2ps -e -l >/tmp/mycal.ps; gv /tmp/mycal.ps &
}

####################################################################
# Full and nearly-full backups.

# Do a full backup. First argument is path to mounted backup directory.
# Second, optional, argument is whether to do a "mini" backup:
# if "mini" it will be a mini backup, if "full" or unset, it will be full.
dobackup() {
    if [[ $# -eq 0 || $1 == '' ]]; then
        echo "Back up to where?"
        return
    fi

    # Exclude these from all backups, even full ones:
    fullexcludes=( Cache .cache Spam LOG log olog Tarballs .Xout \
        VaioWin Bitlbee core outsrc .imap POD .cache/openbox/openbox.log \
        .icons .thumbnails .cache/thumbnails .imap .macromedia core .histfile \
        'VirtualBox VMs' Virtualbox feeds kobo \
        .mozilla/firefox/masterprofile/sessionstore-backups/ \
        webapps store.json.mozlz4 .dbus/session-bus .emacs-saves \
        .config/chromium .googleearth/Temp .googleearth/Cache desert-center )

    # Exclude these from "mini-full" backups (e.g. if low on backup disk space)
    moreexcludes=( '*.mp4' '*.img' '*.iso' DVD \
        outsrc .VirtualBox 'VirtualBox VMs' \
        droidsd-old .googleearth )

    # Build up the excludes list:
    excludesflags=( )
    for ex in $fullexcludes; do
        excludesflags+="--exclude"
        excludesflags+="$ex"
    done

    if [[ $# -eq 2 && $2 == "mini" ]]; then
        echo "Mini backup to" $1
        for ex in $moreexcludes; do
            excludesflags+="--exclude"
            excludesflags+="$ex"
        done
    else
        echo "Full backup to" $1
    fi

    pushd_maybe ~
    echo sudo rsync -av --delete --delete-excluded "${excludesflags[@]}" ./ $1
    sleep 2
    sudo rsync -av --delete --delete-excluded "${excludesflags[@]}" ./ $1
    popd_maybe
}

fullbackup() {
    dobackup "$1" full
}

minibackup() {
    dobackup "$1" mini
}

alias booksync='rsync -av --delete --size-only --exclude .FBReader ~/Docs/droidsd/Books/'

# Always run sqlite inside rlwrap
alias sqlite3="rlwrap -a -z pipeto -i /usr/bin/sqlite3"

# Something is writing to recently-used.xbel and I'm not sure what.
# This might help to monitor it.
alias recent='ls -l ~/recently-used.xbel*(.N) ~/.local/share/recently-used.xbel*'

# Torikun says this might work for talking to the raspberry pi.
# It has something to do with openvpn and might require running a DHCP
# server on the local machine.
alias piroute='iptables -t nat -A POSTROUTING -s 10.8.0.0/24 -o eth0 -j MASQUERADE'

alias tcolors='printf "\e[%dm%d dark\e[0m  \e[%d;1m%d bold\e[0m\n" {30..37}{,,,}'

# I can never remember the ever-changing CUPS commands to talk to
# printers from the cmdline.
alias printers='lpstat -s; echo; echo You can print with lp -d printername'

# Now that we're running feeds on shallowsky.com,
# local/xtra urls have to be saved there too.
# Run with e.g. localurl 'http://blahblah'
# The single quotes are only needed if the URL has an embedded newline,
# like a long URL pasted from mutt or from email from an Apple user.
remove_newlines() {
    # #" expands escape sequences like \n
    echo ${1/$'\n'/}
}

localurl() {
    ( for url in $* ; remove_newlines $url ) | ssh shallowsky.com 'cat >> web/feedme/feeds/localurls'
}

###################################################
# Quick-jump to deeply nested directories
# http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
# Now if I could just remember to use it.
export MARKPATH=$HOME/.marks
function jump { 
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
alias mcd=jump
function mark { 
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark { 
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}
function marks2 {
    ls -l "$MARKPATH" | sed 's:  : :g; s:->:|->:' | cut -d' ' -f9- | column -ts'|'  && echo
}

function _completemarks {
  reply=($(ls $MARKPATH))
}

compctl -K _completemarks jump
compctl -K _completemarks unmark

#### end quick-jump

# Two good pages on zsh scripting:
# http://www.rayninfo.co.uk/tips/zshtips.html
# http://www.linux-mag.com/id/1079/

# alias dumppi='sudo tcpdump -pnvi eth0 -w ~/pi-tcpdump'
#
# On hesiodus:
# tcpdump -pnvi eth0 -w ~/hesiodus.pcap
# ping pi

# Do this on the Pi:
# tcpdump -pnvi wlan0 -w /tmp/.pcap not host 192.168.1.4

# On moon:
# tcpdump -nvi eth0 -w /back/trade/moon.pcap not host 192.168.1.3

# Something keeps changing my stty settings.
# To track it down, check them after every command:
# precmd()
# {
#     stty -a | fgrep -- -ignbrk > /dev/null
#     if [ $? -ne 0 ]; then
#         echo
#         echo "STTY SETTINGS HAVE CHANGED \!\!\!\!\!\!\!\!\!\!\!\!\!\!\!\!"
#         echo
#     fi
# }

# Trying git prompt info:
# http://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html
gitprompt() {
  autoload -Uz vcs_info

  zstyle ':vcs_info:git*' formats "%{$fg[grey]%}%s %{$reset_color%}%r/%S%{$fg[grey]%} %{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%} "

  zstyle ':vcs_info:*' enable git svn
  precmd() {
    vcs_info
  }

  setopt prompt_subst
  PROMPT='${vcs_info_msg_0_}%# '
}

# I can never remember nmap arguments
alias portscan="nmap -v -sT localhost"

# chroot to alternate partition /partitionname in order to update it
chroot-update() {
    partitionname=$1
    titlebar $partitionname chroot
    sudo mount /$partitionname
    sudo mount --bind /proc /$partitionname/proc
    sudo mount --bind /sys /$partitionname/sys
    sudo mount --bind /dev /$partitionname/dev
    sudo mount --bind /dev/pts /$partitionname/dev/pts
    sudo mount --bind /boot /$partitionname/boot
    sudo chroot /$partitionname
}

######################################################
# Source local zsh options:
if [[ -f $HOME/.zshrc.$hostname ]]; then
  . $HOME/.zshrc.$hostname
fi
