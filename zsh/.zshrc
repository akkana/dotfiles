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

# Temporary: disable fetchmail
alias fetchmail='echo NO!!!'

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

# Prevent any repeated entries in $PATH
typeset -U PATH

# Set path
arch=$(uname -m)
export PATH=$HOME/bin:$HOME/bin/linux-$arch:/opt/cxoffice/bin:/usr/local/bin:/usr/local/gimp-git/bin:$PATH:.:/usr/sbin:/sbin:$HOME/outsrc/android-sdk-linux/platform-tools:$HOME/outsrc/android-sdk-linux/tools:$HOME/.local/bin

if (( ! ${+PYTHONPATH} )); then
    export PYTHONPATH=$HOME/bin/pythonlibs
fi

# Autocomplete in the python console:
# https://python.readthedocs.io/en/v2.7.2/tutorial/interactive.html
if [[ -f ~/.pystartup ]]; then
    export PYTHONSTARTUP=~/.pystartup
fi

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
autoxchat()
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

# How many levels deep are we from being a login shell?
# Keep track of that in $primes, to reflect it in the prompt.
if [[ -o login ]]; then
  # A login shell: reset primes.
  export primes=''
else
  # Not a login shell
  export primes=${primes}\'
fi

set_prompt() {
  hostname=$(hostname)

  # Only set this prompt if I'm logged in as myself:
  if [[ $USER == akkana ]]; then
    # PS1=$'%{\e[1m%}<'$(hostname)$primes$'>-%{\e[0m%} '
    #PS1='%K{white}%F{blue}<'$(hostname)$primes$'>- %f%k'

    # If we're on a raspberry pi or similar ARM platform, use a different color:
    # if [[ -f /proc/device-tree/model ]]; then
    if [[ $(uname -a) =~ armv ]]; then
      PS1='%F{red}<'$hostname$primes$'>- %f%k'
    else
      PS1='%F{blue}<'$hostname$primes$'>- %f%k'
    fi

  # But root should have a helpful colorized prompt too:
  elif [[ $USER == root ]]; then
    #PS1=$'%{\e[1m%}#['$(hostname)$primes$']#%{\e[0m%} '
    PS1='%K{white}%F{red}['$hostname$primes$'#]- %f%k'

  # and so should everyone else.
  else
    PS1='%F{green}<'$USER@$hostname$primes$'>- %f%k'
  fi
}

set_prompt


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

export MAILER=mutt
export EDITOR=vim
export VISUAL=vim

# Note: using "$@" instead of $* or $@ in case of sharing code with
# bash users, though in zsh, they're all equivalent and all
# group arguments correctly.

# systemctl pipes through less with some completely broken set of
# arguments that cuts off all output too wide to fit in the terminal.
# Disable that:
# export SYSTEMD_PAGER=
# but it still cuts off at 80 columns, so this works better:
systemctl() {
  /bin/systemctl -l --no-pager "$@"
}

export EDITOR=vim
# Be sure to set bindkey -e -- done above with the other bindkey stuff.

# See http://www.linux-sxs.org/housekeeping/lscolors.html
export LS_COLORS='ex=1;31:ln=1;35'

export RSYNC_RSH=ssh
export PHO_ARGS=-p

#
# Aliases and functions.
#

#
# echo_and_do something.
#
echo_and_do() {
  echo "$@"
  "$@"
}

##################
# Some general aliases:
alias m=mutt
alias j=jobs
alias pd=pushd
# pd() { [[ $# == 0 ]] && set - -; builtin pushd "$@" }
alias s=suspend

alias beep="echo "
alias ap="man -k"

# Run netscheme with sudo -E; it must be root but it also needs access
# to quickbrowse.
alias netscheme='sudo -E /home/akkana/src/netutils/netscheme'

# Distros keeps changing the suspend command; make an alias that won't change:
#alias zzz='sudo /etc/acpi/sleep.sh'
alias zzz='sudo pm-suspend --auto-quirks'

# Newer versions of xterm no longer support titlebar setting with
# the documented sequence of \e]2. But \e]0 works, as long as you
# don't set XTerm*allowSendEvents.
titlebar() {
  # echo ']]2;"$@"'
  echo -e "\033]0; "$@" \007"
}

# Copy the primary selection into the clipboard:
alias primary2clip='xsel -p | xsel -i -b'
# and vice versa:
alias clip2primary='xsel -b | xsel -i -p'

# What's the complement of a number, e.g. the fmask in fstab to get
# a given file mode for vfat files? Sample usage: invert 755
invertmask() {
    python -c "print '0%o' % (0777 - 0$1)"
    # This also works:
    # python -c "print '0%o' % (~(0777 & 0$1) & 0777)"
}

phof () {
    imglist=(`fotogr "$@"`)
    if [[ -z $imglist ]]; then
        echo no match
        return
    fi
    pho $imglist
}

##################
# Alias related to file finding and listing:

show_symlinks() {
    for f in "$@"; do
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
ls() { /bin/ls -FH "$@" ; }
ll() {
    /bin/ls -laFH "$@"
    show_symlinks "$@"
}
llt() { /bin/ls -laSHFLt "$@" ; }
llth() { /bin/ls -lFSHLt "$@" | head -20 ; }

# There are lots of ways to list only directories. Here are some:

# Next two work except for dirs with spaces in the name:
lsdirs1() {
  (cd $1; /bin/ls -d `/bin/ls -1F | grep / | sed 's_/$__'`)
}

lsdirs() {
  echo `/bin/ls -1F $@ | grep / | sed 's_/$__'`| tr -s ' ' '\n' | paste - - - | column -x -t -c3
}

# This one doesn't actually work:
# lsdirs2() { env ls -1FH "$1" | sed -n 's|/$||p' | column; }

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

# Various ways of finding disk hogs:
bigfiles() {
    du -h $1 | grep '[0-9\.]\+G'
    echo
    echo Also consider trying ducks or ncdu
}

# Another big files finder:
ducks() {
    du -cksx $1/* | sort -rn | head -40
}

##################
# Recursive greps
gr() {
  find . -name '*.o' -prune -or -name '*.so' -prune -or -name '*.a' -prune -or -name '*.pyc' -prune -or -name '*.jpg' -prune -or -name '*.JPG' -prune -or -name '*.png' -prune -or -name '*.xcf*' -prune -or -name '*.gmo' -prune -or -name '.intltool*' -prune -or -name '*.po' -prune -or -name 'po' -prune -or -name '*.tar*' -prune -or -name '*.zip' -or -name '.metadata' -or -name 'build' -or -name 'obj-*' -or -name '.git' -prune -or -name '.svn' -prune -or -name '.libs' -prune -or -name __pycache__ -prune -or -type f -print0 | xargs -0 grep "$@" /dev/null
}

zgr() {
  find . -name '*.o' -prune -or -name '*.so' -prune -or -name '*.a' -prune -or -name '*.pyc' -prune -or -name '*.jpg' -prune -or -name '*.JPG' -prune -or -name '*.png' -prune -or -name '*.xcf*' -prune -or -name '*.gmo' -prune -or -name '.intltool*' -prune -or -name '*.po' -prune -or -name 'po' -prune -or -name '*.tar*' -prune -or -name '*.zip' -or -name '.metadata' -or -name 'build' -or -name 'obj-*' -or -name '.git' -prune -or -name '.svn' -prune -or -name '.libs' -prune -or -name __pycache__ -prune -or -type f -print0 | xargs -0 zgrep "$@" /dev/null | fgrep -v .svn | fgrep -v .git
}

cgr() {
  find . \( -name '*.[CchH]' -or -name '*.cpp' -or -name '*.cc' \) -print0 | xargs -0 grep "$@" /dev/null
}
hgr() {
  find . \( -name '*.h' -or -name '*.idl' \) -print0 | xargs -0 grep "$@" /dev/null
}
rgr() {
  find . \( -name '*.rb' -or -name '*.rhtml' \) -print0 | xargs -0 grep "$@" /dev/null | fgrep -v .svn
}
htgr() {
  find . -name '*.*htm*' -and -not -name 'webhits*' -prune -print0 | xargs -0 grep "$@" /dev/null
}
pygr() {
  find . -name '*.py' -print0 | xargs -0 grep "$@" /dev/null
}
jgr() {
  find . -name '*.js' -print0 | xargs -0 grep "$@" /dev/null
}
xgr() {
  find . \( -name '*.cChH' -or -name '*.cpp' -or -name '*.xul' -or -name '*.html' -or -name '*.js' -or -name '*.css' \) -print0 | xargs -0 grep "$@" /dev/null
}
cssgr() {
  find . -name '*.css' -print0 | xargs -0 grep "$@" /dev/null
}
mgr() {
  find . -name '*akefile*' -print0 | xargs -0 grep "$@" /dev/null
}
agr() {
  find . -type f -print0 | xargs -0 grep "$@" /dev/null
}
javagr() {
  find . -name '*.java' -print0 | xargs -0 grep "$@" /dev/null
}
# zgr() {
#  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 zgrep "$@" /dev/null
#}
# Next doesn't work. How do we use -prune?
idagr() {
  find . \( -name OBJ -prune -or -name external -prune -or -name '*scons*' -prune -or -name google_appengine -prune -o -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pyc' \) -print0 | xargs -0 grep "$@" /dev/null | fgrep -v .svn | fgrep -v .git
}

# For some reason, with Tags/Keywords this fails with -print0/-0
# but works without it.
taggr() {
  find . -name 'Tags' -or -name 'Keywords' | xargs grep "$@" /dev/null
}

alias pygrep="langgrep python"

# My PHP projects are scattered among various website images, not ~/bin.
# find is too slow for all of $HOME, so use locate instead.
phpgrep() {
    grep "$@" `locate .php | grep $HOME | grep '\.php$' | egrep -v '(android|index|showpix|ies4linux)'` | sed "s_${HOME}_\~_"
}

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
    pos=("$@"[1,$vterm-1])
    neg=("$@"[$vterm+1,-1])

    cmd="cat"
    for term in $pos; do
        cmd="$cmd | grep $term"
    done
    for term in $neg; do
        cmd="$cmd | grep -v $term"
    done

    zsh -c $cmd
}

# Not really a grep: a crazy filter to use on apache error logs
# to read Flask output without needing a super-wide terminal.
alias flaskfilter="sed 's/^.\{12\}\([0-9]\{2\}:[0-9]\{2\}\):[0-9]\{2\}\.[0-9]\{6\} 20[0-9][0-9]\] \[.*\] \[.*\] \[remote \(.*\)]/\1 (\2)/'"

# End grep aliases

#######################################################
## Keep git repos up to date
#######################################################

# Check which git repos need checkins/pushing:
checkallgit() {
    # Formatting:
    BOLD='\033[1m'
    NONE='\033[00m'
    RED='\033[01;31m'
    out=""

    foreach repo ($myrepos)
        echo "=============================="
        echo -n "=== $repo ... "
        gitbranchsync -cs $HOME/src/$repo
        exitcode=$?
        if [[ $exitcode == 2 ]]; then
            out="$out\n$RED$repo : uncommitted files$NONE"
        elif [[ $exitcode == 1 ]]; then
            out="$out\n$RED$repo : clean but unpushed$NONE"
        elif [[ $exitcode == 0 ]]; then
            out="$out\n$repo: clean"
        fi
    end

    echo
    echo $out
    unset out
}

# Update all my git repositories:
allgit() {
    pushd ~
    # Syntax for zsh arrays, which I always forget:
    badrepos=()
    foreach repo ($myrepos)
        echo "=============================="
        echo "==== $repo :"
        cd ~/src/$repo

        if ! gitbranchsync -ft ; then
            badrepos+=($repo)
        fi
        # Check status here
        if ! git pull --all ; then
            badrepos+=($repo)
        fi
        # Check status here
    end
    popd

    if [[ x$badrepos != x ]]; then
        echo
        echo "=============================="
        echo "Repos with errors:" $badrepos
    else
        echo "No errors"
    fi
}

# If you're working on a branch, and all your changes are committed,
# use this to merge master changes into the current branch.
alias git_merge_branch='git fetch; git rebase origin/master'

# Don't accidentally halt on server machines.
hostname=$(hostname)
if [[ $hostname == 'moon' || $hostname == 'dna' ]]; then
  alias off="echo This is $hostname, you fool!"
  alias reboot="echo This is $hostname, you fool!"
  alias zzz="echo This is $hostname, you fool!"
else
  alias off="sudo poweroff"
  alias reboot="sudo reboot"
  alias zzz="sudo pm-suspend --auto-quirks"
fi

######################################
# audio/video aliases

# mencoder options are black magic.
# This works for converting Minolta quicktime .mov to mpeg:
mov2mpg1() {
  # mencoder has changed its arg structure and this no longer works
  # mencoder $1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg1video -o $2
  echo Sorry, not sure of the new mencoder args
}

# -lavc is ffmpeg, and the default codec is divx:
mov2divx() {
  # mencoder has changed its arg structure and this no longer works
  # mencoder $1 -oac pcm -ovc lavc -o $2
  echo Sorry, not sure of the new mencoder args
}
# From drc on #gimp:
mov2mpeg4() {
  # mencoder has changed its arg structure and this no longer works
  # mencoder $1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg4:vqmin=2:vlelim=-4:vcelim=9:lumi_mask=0.05:dark_mask=0.01:vhq -o $2
  ffmpeg -i $1 -c copy $1:t:r.mp4
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

# Get resolution of a movie file:
moviesize() {
    for f in "$@"; do
        echo "$f:"
        ffprobe -v quiet -print_format json -show_format -show_streams "$1" | egrep '(width|height)'
    done
}

# Playing DVDs with mplayer. f => fullscreen, v -> no subtitles
alias playdvd="mplayer dvd://1 -alang en"

######## end video aliases

########
# Some format conversion commands:

# Prettyprint a JSON file:
ppjson() {
    # This works but reorders the json:
    # python -m json.tool "$@"
    # This doesn't and is just as fast:
    jq . "$@"
}

# Prettyprint XML:
ppxml() {
     xmllint --format "$@"
}

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

gpx2kml() {
    # :t takes the basename, :r removes the extension
    gpsbabel -i gpx -f $1 -o kml -F $1:t:r.kml
}

# gpsbabel by default turns a nice simple track file into a great big
# KML with a waypoint added for each trackpoint. points=0 stops that.
gpx2kmltrack() {
    # :t takes the basename, :r removes the extension
    gpsbabel -i gpx -f $1 -o kml,points=0 -F $1:t:r.kml
}

# Weirdly, gpsbabel can't handle geojson.
gpx2geojson() {
    ogr2ogr -f GeoJSON $1:t:r.geojson $1
}

# ESRI shapefiles to KML. Use the .shp and ignore the other files.
# Converting to GPX usually doesn't work so well; stick with KML.
shp2kml() {
    shapefile=$1
    kmlfile=$shapefile:t:r.kml
    ogr2ogr -f KML $kmlfile $shapefile
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

######## end format changing commands

# Remove the line matching $1 from ~/.ssh/known_hosts.
# Ssh refuses to operate if anything has changed about the host:
# network card, distro it's running, etc.
cleanssh() {
  # mv $HOME/.ssh/known_hosts $HOME/.ssh/known_hosts.bak
  # egrep -v "^\[?$1(\]|\w|\:)" $HOME/.ssh/known_hosts.bak >$HOME/.ssh/known_hosts
  echo Use ssh-keygen -R $1
}

# How many books have I read in recent years?
booksread() {
    setopt extendedglob
    for f in ~/Docs/Lists/books/books[0-9](#c4); do
        year=$(echo $f | sed 's/.*books//')
        let allbooks=$(egrep '^[^ ]' $f | grep -v 'Book List:' | wc -l)
        let rereads=$(egrep '^[-.@\*]' $f  | grep -v 'Book List:'| wc -l)
        # How to do numeric computations in zsh:
        printf "%4s:   All: %3d   New: %3d   Re-reads: %3d\n" \
               $year $allbooks $(($allbooks - $rereads)) $rereads
    done
}

################################################
# Presentations:

# Enable/disable screen blanking.
# Note: xset -q will show settings.

noscreenblank() {
    xset -dpms
    xset s off
}

screenblankon() {
    xset +dpms
    xset s on
}

# Connect to a projector on the VGA port:
alias projector='xrandr --output VGA-1 --mode 1024x768; noscreenblank'
# and on the HDMI port:
# alias projectorh='xrandr --output HDMI1 --mode 1024x768'
# alias projectorh='xrandr --output HDMI-1 --mode 1024x768; noscreenblank'
alias projectorh='xrandr --output LVDS-1 --auto --primary --output HDMI-1 --mode 1024x768; noscreenblank'

# and set video back to normal:
# alias monitor='xrandr --output HDMI1 --mode 1680x1050 --output VGA1 --off --output LVDS1 --off'
alias noprojector='xrandr --auto; screenblankon'
# See also my checkmonitor script.

# Send all audio output to HDMI.
# Usage: hdmisound [on|off], default is on.
# Note: this is unreliable: it switches off after a short time
# and then when it auto-switches back on, you'll miss the first
# few seconds of a sound. Maybe they'll eventually fix that bug.
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
# End external monitor/audio connections

# Large-type versions of terminal and emacs, for presentations:
# alias bigterm="rxvt -geometry 80x33 -fn '-*-lucidatypewriter-*-*-*-*-19-*-*-*-*-*-*-*'"
alias bigterm="rxvt -fn terminus-iso8859-2-bold-18"
# alias noteterm="nohup xterm -geometry 30x34+1025+0 -fn '-*-terminus-bold-*-*-*-22-*-*-*-*-*-*-*' &"
alias noteterm="nohup xterm -geometry 33x37+1025+0 -fn '-*-terminus-bold-*-*-*-20-*-*-*-*-*-*-*' &"

# For notes during planetarium shows:
# red/black for night vision, narrow to show two at once on a laptop.
alias planeterm="nohup rxvt -geometry 62x45 -fn terminus-iso8859-2-bold-18 -bg black -fg red &"

# Make a new empty presentation:
newpreso() {
    if [[ x$1 != x ]]; then
        newdir=$1
    else
        newdir="newpreso"
    fi

    if [[ x$2 != x ]]; then
        fromdir=$2
    else
        fromdir=~/src/htmlpreso
    fi
    echo "Copying from $fromdir"

    if [ ! -d $newdir ]; then
        echo Creating $newdir
        mkdir $newdir
    fi

    cp $fromdir/blank.html $fromdir/img.html $fromdir/navigate.js $fromdir/notes.js $fromdir/slides.css $fromdir/slides.js $fromdir/credits.js $newdir
    mkdir $fromdir/pix

    echo "New presentation files in $newdir"
}

# Making a PDF from a bunch of slides
alias talk2pdf='qhtmlprint $( fgrep .html slides.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '
alias talk2pdf1024='qhtmlprint -1024 $( fgrep .html slides.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '
alias talk2pdf1366='qhtmlprint -1366 $( fgrep .html slides.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '

########## End presentation-related aliases

# Photo alias: Delete all .cr2 files that don't have a corresponding .jpg.
# (That way I can manage my jpgs with metapho and anything deleted, I
# can easily delete the corresponding raw file as well.)
# This doubles as a reminder of how to do fancy pattern subs in zsh scripts.
# Assume the current directory.
delcr2() {
    echo Removing *.cr2(e:'[[ ! -e ${REPLY%.cr2}.jpg ]]':)
    sleep 3
    rm *.cr2(e:'[[ ! -e ${REPLY%.cr2}.jpg ]]':)
}

############################################
# Mount-related aliases

# Mount and df no longer suffice to show mounted filesystems,
# since they show so much irrelevant virtual filesystem crap now.
# Here are ways to clean them up:
mount() {
    if [[ $# -ne 0 ]]; then
        /bin/mount "$@"
        return
    fi

    # Else called with no arguments: we want to list mounted filesystems.
    /bin/mount -t nosysfs,nodevtmpfs,nocgroup,nomqueue,notmpfs,noproc,nopstore,nohugetlbfs,nodebugfs,nodevpts,noautofs,nosecurityfs,nofusectl

    # Two other options, in case that stops working:
    # mount -t ext3,ext4,cifs,nfs,nfs4,zfs
    # mount | grep -E --color=never  '^(/|[[:alnum:]\.-]*:/)'
}

df() {
    if [[ $# -ne 0 ]]; then
        /bin/df "$@"
        return
    fi

    # Else called with no arguments: we want to list mounted filesystems.
    /bin/df -hTx tmpfs -x devtmpfs -x rootfs
}

alias fumount="fusermount -u"

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

# Mount an encrypted disk with cryptmount dev mountpoint.
# alias crypt='cryptmount /dev/something crypt'
# alias uncrypt='cryptunmount crypt'

#########################################
# Raspberry Pi and other embedded computers:

# If we're logged in over a serial port, we might be using screen,
# in which case we need to set the terminal size explicitly:
if [[ $(tty) =~ /dev/ttyAMA0 ]]; then
    termsize
fi

# Serial connections to embedded computers:
#alias plug='minicom -D /dev/ttyUSB1 -b 115200'
alias plug='screen /dev/ttyUSB1 115200'
alias guru='screen /dev/ttyUSB0 115200'
# For the Raspberry Pi, the serial port connections are
# 6=black, 8=white, 10=green
alias rpi='titlebar "Raspberry Pi"; echo "black=Gnd white=TX green=RX"; echo "Disconnect with Ctrl-backquote d"; screen /dev/ttyUSB0 115200; titlebar "local"'
alias pion='titlebar "Raspberry Pi Pion"; ssh -X pi@pion; titlebar "local"'

# Get my current network interface
myif() {
    ip addr show | awk '/inet.*brd/{print $NF}'
}

# Get my current network number, e.g. 192.168.1.0/24
mynet() {
    ip addr show | awk '/inet.*brd/{print $4}'

    # This no longer works now that wlan0 and eth0 are no more
    # addr=$(ip addr show wlan0 2>/dev/null || ip addr show eth0)
    # echo $addr | grep -w inet | awk '{print $2}' | sed 's_\.[0-9]*/\([0-9]*\)_.0/\1_'
}

# Get my current local IP
myaddr() {
    ip addr | grep $(myif) | grep inet | awk '{print $2}' | sed 's_[0-9]*/24_0/24_'
}

# Various ways of getting the current external IP.
# alias myipexternal='curl https://ipinfo.io/ip'
# alias myipexternal="curl -s https://checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'"
# (Also helpful for figuring out how to use wget to write to stdout:)
# alias myipexternal='wget -qO- https://ipinfo.io/ip'
alias myipexternal='dig +short myip.opendns.com @resolver1.opendns.com'

# Find a Raspberry Pi attached to the local network:
localpi() {
    # fping apparently has changed syntax and this no longer works:
    # echo_and_do fping -a -r1 -g $(mynet) |& grep -v Unreachable
    # but this seems to:
    echo_and_do fping -A -d -a -q -g -a -i 1 -r 0 $(myaddr)
    echo
    echo "Now running arp and looking up MACs:"
    echo_and_do arp -n | fgrep " b8:27:eb"
}

# Show everybody connected to the local net:
localnet() {
    echo_and_do fping -A -d -a -q -g -a -i 1 -r 0 $(myaddr)
    echo
    echo "Now running arp and looking up MACs:"
    echo_and_do arp -n |& grep -v incomplete |& mac_lookup
}

# Show everybody on the local net with a specific port open.
# Unfortunately this doesn't show MAC addresses; would be nice to
# combine it with a MAC lookup.
localport() {
    allnet=$(mynet | sed 's_\.[0-9]*/24_.1-254_')
    nmap $allnet -p$1 --open -oG - | grep $1/open
}

# Set up a Linux box to talk to a Pi0 using USB gadget on 192.168.0.7:
pigadget() {
    iface=''
    # How to iterate over a line and set words, without needing an array:
    ip -o link | grep en | grep u | awk '{print $2, $17}' | \
        while read -r iff mac; do
            # LON is a numeric variable containing the digit we care about.
            # The "let" is required so zsh will know it's numeric,
            # otherwise the bitwise test will fail.
            let LON=0x$(echo $mac | sed -e 's/:.*//' -e 's/.//')

            # Is the 2 bit set? Meaning it's a locally administered MAC
            if ((($LON & 0x2) != 0)); then
                iface=$(echo $iff | sed 's/:.*//')
                echo Locally administered bit is set
                break
            else
                echo Locally administered bit is clear
            fi
        done

    if [[ x$iface == x ]]; then
        echo "No enp interface:"
        ip -o link
        echo Bailing.
        return
    fi

    echo "enp interface:" $iface
    echo_and_do sudo ip a add 192.168.7.1/24 dev $iface
    echo_and_do sudo ip link set dev $iface up

    # Enable routing so the gadget can get to the outside world:
    echo_and_do sudo sh -c 'echo 1 > /proc/sys/net/ipv4/ip_forward'
    echo_and_do sudo iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE

    # Install dnsmasq if you want the gadget to have DNS
    # without needing to use 8.8.8.8.
}

# A few electronics cheatsheets:
alias pigpio='pho -P ~/src/pi-zero-w-book/images/raspi-gpio.jpg &'
alias gpio-official='pho -P ~/Docs/hardware/rpi/Pi-GPIO-header.png &'
alias gpio-xyz='pho -P ~/Docs/hardware/rpi/raspberry-pi-pinout.png &'
alias gpio-old='pho -P ~/Docs/hardware/rpi/rpi-gpio.jpg &'
alias resistors='pho -P ~/Docs/hardware/resistors.jpg &'
alias capacitors='quickbrowse ~/Docs/hardware/capacitors.html'
alias voltagedivider='pho -P ~/Docs/hardware/voltage-divider.png &'
alias attiny='pho -P ~/src/arduino/attiny/attiny85-pinout.jpg &'
alias atmega='pho -P ~/Docs/hardware/atmega328-arduino-pinout.jpg &'
alias isp='pho -P ~/Docs/hardware/ISP.png &'
usbtinyisp() {
    echo "MISO	yellow			VCC 	red"
    echo "SCK	white			MOSI 	green"
    echo "RESET	orange, red/black	GND 	black"
    pho -P ~/web/blog/images/hardware/attiny-usbtinyISP_bb.jpg &
    pho -P ~/Docs/hardware/ISP.png &
}

####################################################
# zsh-specific options:

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory notify
unsetopt autocd
# End of lines configured by zsh-newuser-install

# Someone suggested this to avoid losing history on poweroff, etc.
setopt inc_append_history

# This is apparently Ubuntu-specific weirdness:
#skip_global_compinit=1

# The following lines were added by compinstall
#zstyle :compinstall filename '/home/akkana/.zshrc'

# End of lines added by compinstall

# When typing a |, don't eat the space before it.
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;&'

# Older zsh, like on squeeze, don't have compdef.

# zsh annoyingly only prints the last 10 lines of history by default.
# The builtin history command actually calls fc -l which by default
# prints the last 16 commands, but the full form is fc -l first last,
# so fc -l 0 prints everything, fc -l -20 prints the last 20 commands.
history() {
    # Weirdo way to test if stdout is a pipe:
    # If it's a pipe, then print all history, not just the last 80.
    exec 9>&1
    case `readlink /dev/fd/9` in
        pipe:\[*\]) fc -l 0 ;;
    esac

    if [[ x$1 == x ]]; then
        fc -l -80
    elif [[ $1 =~ '-.*' ]]; then
        builtin history "$@"
    else
        builtin history -"$@"
    fi
}

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

################################################
# Build/development helpers

# Run a git with all-default settings.
# Usage: gimpclean VERSION SWM
# e.g. gimpclean git no, gimpclean 2.8 yes
gimpclean() {
    gimpdir=`mktemp -d /tmp/gimpenv.XXXXX`

    # GIMP 2.8 gets very confused about GIMP2_DIRECTORY,
    # and won't create the directories it needs. So we
    # have to create them first.
    for f in brushes dynamics patterns gradients palettes tool-presets; do
        mkdir $gimpdir/$f
    done
    # But, sadly, that's not enough, and 2.8 still won't bring up
    # a window of a reasonable size in either swm or mwm mode.

    if [[ x$1 != x ]]; then
        version=-$1
    else
        version=
    fi
    if [[ x$2 == 'swm' ]]; then
        echo Single Window Mode
    else
        echo Multi Window Mode
        # This doesn't really work, alas.
        echo "(single-window-mode no)" > $gimpdir/sessionrc
    fi
    echo version is $version
    echo "GIMP2_DIRECTORY=$gimpdir gimp$version --new-instance"
    GIMP2_DIRECTORY=$gimpdir gimp$version --new-instance
}

# I get tired of the myriad steps to update gimp now that it
# requires so many repositories.
gimpmaster() {
    # Make sure this exits on errors!
    setopt localoptions errreturn
    export PKG_CONFIG_PATH=/usr/local/gimp-git/share/pkgconfig/
    export CC=/usr/bin/gcc-6

    # Now that mypaint uses a special tag, we can't pull or change anything.
    # pushd_maybe ~/outsrc/libmypaint
    # echo "Updating libmypaint ..."
    # git checkout v1.3.0
    # git pull
    # make -j4
    # make install

    # Getting mypaint-brushes is complicated. Here's what I had to do:
    # git clone https://github.com/Jehan/mypaint-brushes.git
    # cd mypaint-brushes/
    # git checkout --track -b v1.3.x origin/v1.3.x
    # ./autogen.sh --prefix=/usr/local/gimp-git
    # ./configure --prefix=/usr/local/gimp-git
    # But once you do that you can never pull because:
    # "You are not currently on a branch."
    # echo "Updating mypaint-brushes ..."
    # cd ~/outsrc/mypaint-brushes/
    # git pull
    # make -j4
    # make install

    echo "Updating babl ..."
    cd ~/outsrc/babl
    git pull
    make -j4
    make install

    echo "Updating gegl ..."
    cd ~/outsrc/gegl
    git pull
    make -j4
    make install

    echo "Updating GIMP ..."
    cd ~/outsrc/gimp
    git pull
    make -j4
    make install
    popd_maybe

    unset PKG_CONFIG_PATH
    unset CC
}

# update-clone dir repo: if dir already exists, go there and git pull,
# then do a git clean -dfx.
# Otherwise, make the directory and clone the repo into it.
# Either way, we should end up in the directory with an up-to-date repo.
# The third, branch argument has never been tested.
pull-clone-clean() {
    repo=$1
    d=$2
    branch=$3
    echo repo $repo, d $d, branch $branch
    if [ -d $d ]; then
        cd $d
        git pull
        git clean -dfx
    else
        if [[ x$branch == x ]]; then   # no branch
            cd $(dirname $d)
            pwd
            git clone $repo
        else
            git clone -b $branch $repo
            cd $(dirname $d)
            git checkout $branch
        fi
        cd $d

    fi
}

# When GIMP needs to be rebuilt from scratch --
# this also serves as a cheatsheet for requirements.
#
# Testing tip: pkg-config --prefix-variable=$PREFIX --modversion libmypaint-1.0
# (or whatever package you're having problems with).
gimpmaster-fresh() {
    # Make sure this exits on errors:
    setopt localoptions errreturn

    PREFIX=/usr/local/gimp-git
    SRCDIR=$HOME/outsrc

    # Overwrite config.site:
    mkdir -p $PREFIX/share/
    cat >$PREFIX/share/config.site <<EOF
export PATH="$PREFIX/bin:\$PATH"
export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PREFIX/share/pkgconfig:\$PKG_CONFIG_PATH"
export LD_LIBRARY_PATH="$PREFIX/lib:\$LD_LIBRARY_PATH"
export ACLOCAL_FLAGS="-I $PREFIX/share/aclocal \$ACLOCAL_FLAGS"
EOF

    # libmypaint is super precarious: need to pull a specific tag and
    # end up in detached head state, and then don't ever pull again.
    if [ ! -d $SRCDIR/libmypaint ]; then
        git clone https://github.com/mypaint/libmypaint.git
        cd $SRCDIR/libmypaint
        git checkout v1.3.0
    else
        cd $SRCDIR/libmypaint
        if [[ -f Makefile ]]; then
          make clean
        fi
    fi
    ./autogen.sh --prefix=$PREFIX
    ./configure --prefix=$PREFIX
    make
    make install

    echo "================ mypaint-brushes"
    pull-clone-clean https://github.com/Jehan/mypaint-brushes.git $SRCDIR/mypaint-brushes
    git checkout v1.3.0
    echo "================ autogen mypaint-brushes --prefix=$PREFIX"
    ./autogen.sh --prefix=$PREFIX
    echo "================ configure mypaint-brushes --prefix=$PREFIX"
    ./configure --prefix=$PREFIX
    echo "================ make mypaint-brushes"
    make
    echo "================ make install mypaint-brushes"
    make install

    echo "================ babl"
    pull-clone-clean https://gitlab.gnome.org/GNOME/babl.git $SRCDIR/babl
    echo "================ autogen babl --prefix=$PREFIX"
    ./autogen.sh --prefix=$PREFIX
    echo "================ make babl"
    make
    echo "================ make install babl"
    make install

    echo "================ gegl"
    pull-clone-clean https://gitlab.gnome.org/GNOME/gegl.git $SRCDIR/gegl
    echo "================ autogen gegl --prefix=$PREFIX"
    ./autogen.sh --prefix=$PREFIX
    echo "================ make gegl"
    make
    echo "================ make install gegl"
    make install

    echo "================ gimp"
    pull-clone-clean https://gitlab.gnome.org/GNOME/gimp.git $SRCDIR/gimp
    echo "================ autogen gimp --prefix=$PREFIX"
    ./autogen.sh --prefix=$PREFIX
    echo "================ make gimp"
    make
    echo "================ make install gimp"
    make install

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

# Build a new copy of my forked hexchat.
# This frequently fails with meson/ninja errors because those tools
# are always updating and changing formats.
# If that happens, the suggested commands never work;
# instead remove or rename the build directory.
newhexchat() {
    # Make sure this exits on errors from here on.
    setopt localoptions errreturn

    pushd_maybe ~/outsrc/hexchat

    # Pull changes from upstream and merge into my fork.
    # Should also push these changes back to my fork.
    # https://help.github.com/articles/syncing-a-fork/
    git fetch upstream
    git checkout master
    git merge upstream/master

    # make -j4
    # make install

    # http://hexchat.readthedocs.io/en/latest/building.html#unix
    meson build
    ninja -C build
    ninja -C build install

    popd_maybe
}

# Reminder for building firefox.
# Most of the time this will probably require manual intervention.
newfox() {
    # Make sure this exits on errors!
    setopt localoptions errreturn

    pushd_maybe ~/outsrc/gecko-dev/

    echo_and_do git pull
    # Sadly, should do a clobber each time, since I build firefox infrequently.
    # The build almost always has something that breaks due to missing deps.
    echo_and_do ./mach clobber
    echo_and_do ./mach build
    echo_and_do ./mach package
    echo "Tarball should be in" `pwd`/obj*/dist/firefox/
}

# Building Firefox tends to overload the CPU, sometimes causing thermal
# shutdown. Since Linux, amazingly, seems to have no way to automatically
# throttle back processes based on CPU temp, here's a way to keep an
# eye on it manually:
tempwatch() {
    while true; do
        echo ''
        sensors | grep Core
        sleep 10
    done
}

#############################################################
# Debian apt helpers.

# There doesn't seem to be any way to exclude all those i386 packages
# when searching, so search results are twice as long as they need to be.
# Also, there's no easy way to search for only installed packages.
# There are ways, but they're quite hard to type:
# packages whose name contains bash: aptitude search '~i bash'
# also, aptitude search '?narrow(?installed, …)'
# packages whose description contains bash: aptitude search '~i ~d bash'
# packages that are not installed: aptitude search '!~i bash'
#
# aptitude's notion of "description" is the long description, not the
# short one that shows up in aptitude search. So if you search on ~d
# (-D in this function) you'll get matches that don't include the search
# term anywhere. -d in this function implies -D but then greps the output
# to ensure the search term is there.
#
# aptitude search reference:
# https://www.debian.org/doc/manuals/aptitude/ch02s04s05.en.html
aptsearch() {
    as_usage() { echo "Usage: aptsearch [-idD] pattern" }

    local OPTIND o only_installed description
    while getopts ":idD" o; do
        case "${o}" in
            i)
                only_installed=1
                ;;
            d)
                shortdesc=1
                description=1
                ;;
            D)
                description=1
                ;;
            *)
                aptsearch_usage
                return
                ;;
        esac
    done
    shift $((OPTIND-1))

    if [[ $only_installed ]]; then
        argstr='~i '
    else
        argstr=''
    fi

    if [[ $description ]]; then
        argstr="${argstr}~d "
    fi

    argstr="${argstr} $*"

    if [[ $shortdesc ]]; then
        # restrict output to lines actually containing the search term
        echo "aptitude search \"${argstr}\" | grep $1"
        echo
        aptitude search "${argstr}" | grep "$1"
    else
        echo "aptitude search \"${argstr}\""
        echo
        aptitude search "${argstr}"
    fi
}

# Debian apt: Check on status of all held packages:
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

#############################################################
# Python virtualenv and path helpers.

# Python virtualenvs for everyday use.
# pip install --user doesn't work properly on Debian: it ignores
# system-installed packages and re-installs dependencies that don't
# need re-installing, https://github.com/pypa/pip/issues/4222
# So instead, use a virtualenv all the time to do the job .local
# was supposed to do.
# (.local isn't a good solution anyway if you need both python2 and
# python3, since the packages will overwrite each other.)
#
# Set each one up once with:
# virtualenv --system-site-packages $HOME/.python2env-$archbits (python2)
#   (requires virtualenv and python-virtualenv)
# python3 -m venv --system-site-packages .python3env-$archbits (python3)
#   (requires python3-venv)
#
# If you need a specific version of Python, try e.g.:
# sudo apt-get install python3.6 python3.6-venv
# python3.6 -m venv ~/pyenv/py3.6-tf
# Some sources say you can use venv --python=/usr/bin/python3.6,
# but it fails in some cases.


nopythonenv() {
    if type deactivate >/dev/null ; then
        deactivate
    fi
    if type deactivate >/dev/null ; then
        deactivate
    fi
    set_prompt
}

switchpythonenv() {
    nopythonenv

    if [[ x$1 == x ]]; then
        vers=2
    else
        vers=$1
    fi
    if [[ $arch == 'x86_64' ]]; then
        archbits=64
    else
        archbits=32
    fi

    if [[ $vers == 23 ]]; then
        echo Using both Python 2 and 3 envs on ${archbits} bit
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $HOME/.python2env-${archbits}/bin/activate
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $HOME/.python3env-${archbits}/bin/activate
    else
        echo Switching Python envs to python${vers}, ${archbits} bit
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $HOME/.python${vers}env-${archbits}/bin/activate
    fi

    set_prompt
    PS1="%F{magenta}Py${vers} ${PS1}"
}

alias python2env='switchpythonenv 2'
alias python3env='switchpythonenv 3'
alias python23env='switchpythonenv 23'

# Make a temporary Python virtualenv for testing something.
# This is separate from the virtualenvs used for everyday life.
# Remove all special PATH and PYTHONPATH elements like ~/bin.
# This still leaves the problem of ~/.local, which pip and venv
# will use in preference to the venv.
venv() {
    export PATH=''
    export PYTHONPATH=''
    . /etc/zsh/zshenv
    virtualenv --system-site-packages venv
    . venv/bin/activate
}

venv-nosite() {
    export PATH=''
    export PYTHONPATH=''
    . /etc/zsh/zshenv
    virtualenv venv
    . venv/bin/activate
}

# Where would a python module be imported from?
pythonwhich() {
    foreach lib ("$@")
      python -c "import imp; file, pathname, description = imp.find_module(\"$lib\"); print pathname"
    end
}

#############################################################
# Python help functions. Get help on a Python class in a
# format that can be piped through grep, redirected to a file, etc.
# Usage: pythonhelp [module.]class [module.]class ...
# Turns out there's already a program for that: pydoc a.b.c
pythonXhelp() {
    python=$1
    shift
    for f in "$@"; do
        if [[ $f =~ '.*\..*' ]]; then
            module=$f:r
            obj=$f:e
            s="from ${module} import ${obj}; help($obj)"
        else
            module=''
            obj=$f
            s="help($obj)"
        fi
        $python -c $s
    done
}
alias pythonhelp="pythonXhelp python"
alias python2help="pythonXhelp python2"
alias python3help="pythonXhelp python3"

alias unittest='python3 -m unittest discover'
alias unittest3='python3 -m unittest discover'
alias unittest2='python2 -m unittest discover'

#############################################################
# Android-related aliases

# Some aliases for getting files from Android KitKat via adb,
# since the lack of usb-storage and autocomplete is such a pain.

# Where is the SD card on my phone?
# Under KitKat, it's at /storage/extSdCard.
# Under Marshmallow, it's at /storage/nnnn-nnnn
# Set androidSD in .zshrc.hostname: these aliases require it.

# There seems to be no way to remove multiple or wildcarded files via adb.
# alias delallgpx='adb shell rm /mnt/extSdCard/Android/data/net.osmand.plus/tracks/rec/*'

pullgpx() {
  pushd_maybe ~/Docs/gps/new
  adb pull $androidSD/Android/data/net.osmand.plus/files/tracks/rec/. .
  for f in *.gpx; do
    echo $f
    adb shell rm $androidSD/Android/data/net.osmand.plus/files/tracks/rec/$f
  done
  ls
  echo Maybe adb push file.gpx $androidSD/GPX/
  echo Maybe adb push file.gpx $androidSD/Android/data/net.osmand.plus/files/tracks/
}

pullphotos() {
  pushd_maybe ~/Docs/gps/new
  adb pull $androidSD/DCIM/Camera/. .
  # adb pull /storage/sdcard0/DCIM/CardboardCamera/. .
  setopt extendedglob
  setopt EXTENDED_GLOB
  for f in *.jpg~*.vr.jpg *.mp4; do
    echo $f
    adb shell rm $androidSD/DCIM/Camera/$f
  done
  # If we start shooting a lot with CardboardCamera, can delete those too.
  echo "Pulled photos:"
  ls
}

pullscreenshot() {
  pushd_maybe ~/Docs/gps/new
  adb pull /sdcard/Pictures/Screenshots/. .
  for f in *.png; do
    echo $f
    adb shell rm /sdcard/Pictures/Screenshots/$f
  done
}

# But what if we don't have adb installed? Here's how to do it using gphoto2.
# Set androidDCIM in .zshrc.hostname to something like /store_00020002
# but unfortunately I don't know how to get the magic number.
alias pullphotosg='gphoto2 --folder $androidDCIM/DCIM/Camera -P'
alias delphotosg='gphoto2 --folder $androidDCIM/DCIM/Camera -D'

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
adebug() {
    adb logcat | egrep --line-buffered "($1|E/AndroidRuntime)" | grep -v --line-buffered Delivering
}
adebug1() {
    adb logcat -s ActivityManager:I AndroidRuntime:E $1:D '*:S'
}
alias debugfeed='adebug Feed'
# alias df2='adb logcat | egrep --line-buffered "(Feed|E/AndroidRuntime)" | grep -v --line-buffered Delivering'

# For building Android apps such as osmand:
export ANDROID_HOME=$HOME/outsrc/android-sdk-linux
export ANDROID_SDK=$HOME/outsrc/android-sdk-linux
export ANDROID_NDK=$HOME/outsrc/android-ndk-r10d

# Find location of Android imports, when writing code:
andimport() {
    find $ANDROID_SDK -name $1.java
}

# End Android

##################################
# Spam and email-related aliases

# Tail the procmail log file, for when I'm expecting mail:
alias proctail="tail -1000f Procmail/log | egrep -v '^procmail'"

# Spast checks spam with e.g. echo $subj | grep -i -f $patfile
# How do we find out from $subj which line in $patfile matched the grep?
# Sample Usage: whichspam 'subject-line' subjectRejects
whichspam() {
  # to print each line before executing, for debugging purposes:
  set -o xtrace
  whichfile=$2
  if [[ x$whichfile == x ]]; then
    whichfile=subjectRejects
  fi
  echo Searching in ~/Procmail/spast/$whichfile for "$1"
  cat ~/Procmail/spast/$whichfile | while read line ; do
    #echo echo "$1" '| egrep -i --' "$line" '>/dev/null'
    #echo "$1" | egrep -i -- "$line" >/dev/null
    # echo "$1" PIPE egrep -i -- "$line"
    echo "$1" | egrep -i -- "$line"
    if [[ $? == 0 ]]; then
      echo $line
    fi
  done
  set +o xtrace
}

# Sometimes editing one of the files accidentally produces a blank line,
# which causes tons of legitimate mail to be misfiltered.
# Check for that:
check-spam-blanks() {
    if [ ! -d ~/Procmail/spast ]; then
        return
    fi
    pushd ~/Procmail/spast >/dev/null
    output=$(egrep -s '^$' *)
    if [[ x$output != x ]]; then
        echo '************ Yikes! Blank lines in ~/Procmail/spast:'
        echo $output
    fi
    popd >/dev/null
}

#
# Search for spam subjects or from lines in Spam/saved,
# for purposes of telling which patterns should be added to procmail filters.
#
# Note: this is one of the few cases in zsh where there's a significant
# difference between "$@" and "$*", and "$*" is needed here.
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
    # but save the old headers (not message bodies) for spam filter
    # development purposes.
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
    rm -f $HOME/Procmail/log
}
############ End spam-related aliases

##################################

#
# PyBlosxom helpers for my blog:
#
blogupdate() {
  pushd_maybe ~/web/blogfiles
  setopt localoptions errreturn
  pyblosxom-cmd staticrender --incremental
  ~/bin/blogtopics
  mv ../blog/topics.html ../blog/oldtopics.html
  mv ../blog/newtopics.html ../blog/topics.html
  blog-tag-index
  popd_maybe
}

blogup() {
  pushd_maybe ~/web/blogfiles
  setopt localoptions errreturn
  pyblosxom-cmd staticrender --incremental
  popd_maybe
}

# Sync new blog files back to the server:
alias blogsync='rsync -av --delete ~/web/blog ~/web/blogfiles shallowsky.com:web/'

# End PyBlosxom helpers.

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

    # Exclude files/dirs with these names from all backups, even full ones:
    fullexcludes=( Cache ".cache/*" core Spam LOG log olog foo .Xout feeds \
        .local .pythonenv Tarballs \
        desert-center planetarium-movies \
        ebirddata \
        VaioWin core outsrc .imap \
        .icons .thumbnails .cache/thumbnails .imap .macromedia .histfile \
        .gradle/ .dbus/ .emacs-saves \
        .config/chromium .googleearth/Temp .googleearth/Cache \

        # All the places virtualbox stores profiles:
        'VirtualBox VMs' Virtualbox .VirtualBox \

        # All the various crap firefox stores:
        "*.Default User" \
        "gmp-gmp*"/ crashes/ datareporting/ '/healthreport.sqlite*' \
        'webapps*' \
        storage/permanent/ storage/default/ storage/temporary/ \
        sessionstore-backups/ saved-telemetry-pings/ "*store.json*" \

        )

    # Exclude these from "mini-full" backups (e.g. if low on backup disk space)
    moreexcludes=( '*.mp4' '*.img' '*.iso' DVD \
        outsrc kobo planetarium-movies \
        droidsd-old .googleearth )

    # Things we want even though they're under otherwise excluded patterns.
    # If these are part of patterns that would otherwise be excluded,
    # use a * in the exclude pattern.
    # E.g. include .cache/feedme/, exclude .cache/* instead of .cache/
    includes=( .cache/feedme/ outsrc/gimp/ outsrc/hexchat/ outsrc/openbox/ )

    # Build up the excludes list:
    excludesflags=( )

    for i in $includes; do
        excludesflags+="--include"
        excludesflags+="$i"
    done

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

    if [[ ! -f $1/.config/zsh/.zshrc ]]; then
        echo
        echo "********************************************************"
        echo "WARNING: $1 doesn't appear to be an existing backup dir."
        echo "Are you sure?"
        echo "Waiting for 10 seconds ..."
        echo "********************************************************"
        echo
        sleep 10
    fi
    echo sudo rsync -av --delete --delete-excluded "${excludesflags[@]}" ./ $1
    sleep 2
    pushd_maybe ~
    sudo rsync -av --delete --delete-excluded "${excludesflags[@]}" ./ $1
    popd_maybe
}

#
# Usage: fullbackup target, e.g. fullbackup /backupdisk/username/
#
fullbackup() {
    dobackup "$1" full
}

minibackup() {
    dobackup "$1" mini
}

############# end backups

####################################################################
# Rsync local files up to a web server
# Usage: towebhost dir
# $webserver will be used as the webserver default, if not specified.
# Set up pre-defined web hosts and their local and remote paths as follows:
# webhosts=(      mywebserver1.com mywebserver2.com )
# weblocalpaths=( $home/mywebdir   /public/myotherwebdir )

towebhost() {
    fromwebhost=0
    flags=''
    # For testing, this is a convenient place to force -n
    # flags='-n'

    # Evaluate arguments: -n and/or -f
    for i in "$@"
    do
        case $i in
            -n)
                flags='-n'
                shift
                ;;
            -f)
                fromwebhost=1
                shift
                ;;
            --default)
                break
                ;;
        esac
    done

    if [[ $# == 0 ]]; then
        print "Usage: towebhost file_or_dir"
        return
    fi


    excludes="--exclude .git --exclude cache --exclude __pycache__ --exclude '*.pyc'"

    for dst in "$@"; do
        echo $dst

        # Get the full path of the argument:
        localpath=$dst:A

        # Sanity check our three webhosts variables:
        if [[ $#weblocalpaths != $#webhosts ]]
        then
            echo "Error: webhosts and weblocalpaths don't match"
            return
        fi
        webhost='none'
        for i in {1..$#webhosts}; do
            if [[ $localpath == $weblocalpaths[$i]* ]]; then
                webhost=$webhosts[$i]
                localbase=$weblocalpaths[$i]
                break
            fi
        done

        if [[ $webhost == 'none' ]]; then
            echo "$localpath doesn't match any known local path in $weblocalpaths"
            return
        fi

        # Make sure directories have a terminal slash,
        # whether or not the user provided one.
        if [ -d $localpath ]; then
            # Remove terminal slash.
            ## requires extendedglob, so make sure it's set locally.
            setopt localoptions extendedglob
            localpath=${localpath%%/##}/
        fi

        remotepath=${localpath#$localbase}

        if [[ $fromwebhost == 1 ]]; then
            echo "Copying from $webhost$remotepath to local $localpath"
            echo
            cmd="rsync -av $flags --delete $excludes $webhost$remotepath $localpath"

        else
            echo "Copying $localpath to $webhost$remotepath"
            echo
            cmd="rsync -av $flags --delete $excludes $localpath $webhost$remotepath"
        fi

        echo $cmd
        eval $cmd
    done
}

fromwebhost() { towebhost -f $@ }

# Sync source directories to the same place on another machine,
# relative to $HOME. Exclude git information, .pyc etc.
# Usage: srcsync . otherhostname
srcsync() {
    # Get the full path of the argument:
    localpath=$1:A
    remotehost=$2
    if [[ $localpath == '' || $remotehost == '' ]]; then
        echo "Usage: srcsync localpath remotehostname"
        return
    fi

    # Strip off $HOME
    remotepath=${localpath#$HOME/}

    echo_and_do rsync -av --delete --exclude .git \
                --exclude '*.pyc' --exclude __pycache__ \
                $localpath/ $remotehost:$remotepath/
}

####################################################################
# Some less-used general aliases:

# Linux has a lovely list of all compose key sequences.
composekey() {
  grep -i $1 /usr/share/X11/locale/en_US.UTF-8/Compose ~/.XCompose
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

alias akk="aplay $HOME/.xchat2/sounds/akk.wav"
alias screenshot="scrot -b -s screenshot.jpg"
alias thes="dict -h localhost -d moby-thesaurus"

# Spellcheck
sp() {
  spell "$@" | sort | uniq
}

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

##### printers ################

# Nice overview of lp options: https://www.computerhope.com/unix/ulp.htm

# Which printers are available? lpstat -p -d also works.
alias whichprinters='lpstat -a; echo "print with lp -d dest -n num-copies"; echo "For PDF consider adding -o fit-to-page or -o scaling=100, or using pdfpages or pdfjam"'

# Print duplex on a printer that supports it.
# If using this with -n num_copies, be sure to add -o collate=true too,
# or else you'll get a sheet with page 1 on both sides, etc.
duplexlp() {
    lp -o sides=two-sided-long-edge -o collate=true "$@"
}
duplexbrother() {
    lp -o sides=two-sided-long-edge -o collate=true -d Brother_HL-3170CDW "$@"
}
duplexdell() {
    lp -o sides=two-sided-long-edge -o collate=true -d Dell_Printer_E310dw "$@"
}

# lp inconsistently decides to use zero margins. When it does, this helps.
# (In theory, adding -o page-top=17 should add a top margin, but in
# 2014 this seems to make a negative margin, dropping the first few
# lines. All hail Linux printing!)
alias lpp='lp -o page-left=38'

##### dates ################

# What's the current time in UT / GMT?
ut() {
    date -u "$@"
}

# Convert a fixed date (e.g. for a meeting) from UT/GMT.
# date -d 'Tue November 12 18:00 UTC' or date -d '18:00 UTC next Friday'
fromut() {
    date -d "$@"
}

# Subtract dates
datediff() {
    d1=$(date -d "$1" +%s)
    d2=$(date -d "$2" +%s)
    echo $(( (d1 - d2) / 86400 )) days
    echo $(( (d1 - d2) / 86400 / 7. )) weeks
    echo $(( (d1 - d2) / 86400 / 7 )) weeks $(( (d1 - d2) / 86400 % 7 )) days
}

dayofweek() {
    echo -n "Date as YYYY-MM-DD: "
    read d
    date --date=$d +%a
}

# Tell aptitude not to limit descriptions to the terminal width
alias aptitude='/usr/bin/aptitude --disable-columns'

# Adjust for day or nighttime monitor modes
alias day="xrandr --output HDMI1 --brightness 1.0"
alias night="xrandr --output HDMI1 --brightness .8"

# Run ebook programs, except that neither of them work in wine any more:
alias kindle="wine ~/.wine/drive_c/Program\ Files/Amazon/Kindle/Kindle.exe"
# alias adobeDE="wine ~/.wine/drive_c/Program\ Files/Adobe/Adobe\ Digital\ Editions/digitaleditions.exe"
# alias adobeDE="cxrun ~/.cxoffice/ADE_4/drive_c/Program\ Files/Adobe/Adobe\ Digital\ Editions\ 4.5/DigitalEditions.exe"
alias adobeDE="wine .wine/drive_c/Program\ Files\ \(x86\)/Adobe/Adobe\ Digital\ Editions1.7.2/digitaleditions.exe"
# To open something like a URLLink.acsm file: wine start URLLink.acsm

# R has no way to tell it not to prompt annoyingly to save the environment
# every time you quit, except as a commandline flag:
alias R="/usr/bin/R --no-save"

# Convert temperatures between F and C, because units' crazy syntax
# is impossible to remember. (If ctemp isn't installed.)
# Of course a python, perl or awk one-liner would be easier,
# but this is a reminder of how to do it with units.
c2f() {
    units "tempC($1)" tempF
}
f2c() {
    units "tempF($1)" tempC
}

# Always run sqlite inside rlwrap to get better commandline editing:
alias sqlite3="rlwrap -a -z pipeto -i /usr/bin/sqlite3"

# Which numbers correspond to which colors on this terminal?
alias tcolors='printf "\e[%dm%d dark\e[0m  \e[%d;1m%d bold\e[0m\n" {30..37}{,,,}'

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

######################################################
# Source local, machine-specific zsh options:
if [[ -f $HOME/.config/zsh/.zshrc.$hostname ]]; then
  . $HOME/.config/zsh/.zshrc.$hostname
fi
