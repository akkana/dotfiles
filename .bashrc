#########################
# Akkana's .bashrc
#########################

# User specific aliases and functions

# Get noninteractive shells out of here
# even though the bash documentation says noninteractive
# shells aren't supposed to execute .bashrc according to
# http://www.faqs.org/docs/bashman/bashref_63.html#SEC63
if tty -s
then
 :
else
  return
fi

# echo .bashrc

# Source global definitions
if [[ -f /etc/bashrc ]]; then
    . /etc/bashrc
fi

# Word erase only back to punctuation.
# This also requires that .inputrc include:
# set bind-tty-special-chars Off
bind '\C-w:backward-kill-word'

# Set path
export PATH=$HOME/bin:$HOME/bin/linux:/usr/local/bin:/usr/sbin:/usr/bin:/bin:/usr/bin/X11:.:/opt/kde/bin:/sbin:/usr/games

ulimit -c unlimited

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

PS1="\[$standout\][\h$primes]-\[$standout_end\] "
export primes=${primes}\'

# Bash defaults to a really short timeout, and exits on inactivity
TMOUT=0

# Environment
export PAGER=less
export LESS="-EerX"
export EDITOR=vim
export LC_COLLATE=C

# See http://www.linux-sxs.org/housekeeping/lscolors.html
export LS_COLORS='ex=1;31:ln=1;35'

export RSYNC_RSH=ssh
export PHO_ARGS=-p

# aliases and functions

unalias ls
ls() { /bin/ls -aF --color $* ; }
unalias ll
ll() { /bin/ls -laF --color $* ; }
llt() { /bin/ls -laSFHLt --color $* ; }
llth() { /bin/ls -laFSHLt --color $* | head ; }

alias j=jobs
alias m=mutt
alias pd=pushd
alias s=suspend

titlebar() {
  echo ']2;$*'
}
alias rl="telnet -r"

alias beep="echo "
alias akk="play $HOME/.xchat2/sounds/akk.wav"
alias ph="'grep -i \!* ~/.../Phones'"
alias ap="man -k"

alias screenshot="import -window root screenshot.jpg"
alias thes="dict -h localhost -d moby-thesaurus"

alias newbg='xsetbg -fullscreen -border black `find -L $HOME/Backgrounds -name "*.*" | randomline`'

alias akk='play ~/.xchat2/sounds/akk.wav'

# Spelling check
sp() {
  spell $* | sort | uniq
}

###############################
# Recursive greps
gr() {
  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 grep $* /dev/null | fgrep -v .svn
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
zgr() {
  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 zgrep $* /dev/null
}

# halt and reboot don't always work on the Vaio,
# and can't be run suid.
alias off="sudo shutdown -h now"
alias halt="sudo shutdown -h now"
alias reboot="sudo shutdown -r now"

# Get ink status from Epson printers
alias epsonink="escputil -r /dev/usblp0 -mC86 -u -i"
alias epsonnozzle="escputil -r /dev/usblp0 -mC86 -u -n"
alias epsonclean="escputil -r /dev/usblp0 -mC86 -u -c"

# blog stuff
alias blogupdate='cd ~/web/blogfiles && ./pyblosxom.cgi --static --incremental && ~/bin/blogtopics && mv ../blog/topics.html ../blog/oldtopics.html && mv ../blog/newtopics.html ../blog/topics.html && cd'

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

# Remove the line matching $1 from ~/.ssh/known_hosts.
# Ssh refuses to operate if anything has changed about the host:
# network card, distro it's running, etc.
cleanssh() {
  mv $HOME/.ssh/known_hosts $HOME/.ssh/known_hosts.bak
  grep -v $1 $HOME/.ssh/known_hosts.bak >$HOME/.ssh/known_hosts
}

# Get the temperature from /proc/acpi/thermal_zone/THRM/temperature
# and convert it to F
alias temp="cat /proc/acpi/thermal_zone/ATF0/temperature"

# Record a realaudio stream
getreal() {
  mplayer -playlist $1 -ao pcm:file=$2 -vc dummy -vo null
}
# Then transcode it with: 
# lame --tg Other --ta artist -tl album file.wav file.mp3

alias ratings='links -dump "http://www.amazon.com/gp/product/1590595874" | grep "in Books" | grep -v Explore'

# For presentations
alias bigterm="rxvt -geometry 80x33 -fn '-*-lucidatypewriter-*-*-*-*-19-*-*-*-*-*-*-*'"

# Starting system software:
alias apache="sudo /etc/noinit.d/apache2"
alias cups="sudo /etc/noinit.d/cupsys"
alias mysql="sudo /etc/noinit.d/mysql"

if [[ $HOSTNAME != 'moon' ]]; then
  alias rlm='ssh moon'
  alias xx=startx
fi

# allanbrokeit on #archlinux says this might cure the mysterious
# disappearing history. But it doesn't.
shopt -s histappend

if [[ -e $HOME/.bash-errs ]]; then
  . $HOME/.bash-errs
fi

# function myeditor {
#   if [[ x"$VISUAL" != x ]]; then
#     echo "$VISUAL"
#   elif [[ x"$EDITOR" != x ]]; then
#     echo "$EDITOR"
#   else
#     echo e.g. vim
#   fi
# }

# Trap errors.
# This could also be done, less cleanly, via PROMPT_COMMAND
# by checking $? for nonzero status. fc -n -l -1
#trap 'err_handle' ERR
#trap 'err_handle $(history | tail -1 | sed \'s/^ *[0-9][0-9]* *//\')' ERR
#trap 'err_handle "$(fc -n -l -0)"' ERR
#trap 'err_handle "$BASH_COMMAND"' ERR

# OHHH! The reason lines disappear from my history
# is probably that bash lets you edit history lines
# and remembers their edited value, not the original one.
# This may be controlled by 'histreedit'.
# Or it might not.

# Override the Ubuntu way, if desired:
#function command_not_found_handle {
# return 127
#}

if [ -f ~/ideascopic/it/cheats/bash/cheats.bash ]; then
  source ~/ideascopic/it/cheats/bash/cheats.bash
fi

alias plug='minicom -D /dev/ttyUSB1 -b 115200'
