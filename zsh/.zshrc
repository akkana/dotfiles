#! /usr/bin/zsh

#########################
# Akkana's .zshrc
#########################

# When debugging problems:
# echo '============================== .zshrc'
# set -x

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
# Temporary: warn about exifautotran
alias exifautotran-"echo exifautotran is broken, use jhead -autorot; echo See https://bugs.launchpad.net/ubuntu/+source/libjpeg9/+bug/1842116"

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
# But here's a better solution that lets me know when there's a problem.
# It needs to always return false except in the -e case.
# command_not_found_handler() { echo $? && return 1 };
# This also works to print exit codes but is more verbose:
setopt printexitvalue
# Or this much more elaborate solution:
# . $HOME/.zsh.printexit

# Prevent any repeated entries in $PATH
typeset -U PATH

ulimit -c unlimited

# Allow pasting functions with comments
# However, this interferes with being able to use # on the commandline.
setopt interactivecomments
# Also, you can't use # on the commandline because it's a special zsh global.
# To use it:
# unsetopt extendedglob

# Prompt setting

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
  hostname=$(hostname -s)
  whoami=$(whoami)

  # Only set this prompt if I'm logged in as myself:
  if [[ $whoami == akkana ]]; then
    # PS1=$'%{\e[1m%}<'$(hostname)$primes$'>-%{\e[0m%} '
    #PS1='%K{white}%F{blue}<'$(hostname)$primes$'>- %f%k'

    # If we're on a raspberry pi or similar ARM platform, use a different color:
    # if [[ -f /proc/device-tree/model ]]; then
    if [[ $(uname -a) =~ armv ]]; then
      PS1='%F{red}%B<'$hostname$primes$'>- %b%f%k'
    else
      PS1='%F{blue}%B<'$hostname$primes$'>- %b%f%k'
    fi

  # But root should have a helpful colorized prompt too:
  elif [[ $whoami == root ]]; then
    #PS1=$'%{\e[1m%}#['$(hostname)$primes$']#%{\e[0m%} '
    PS1='%K{white}%F{red}%B['$hostname$primes$'#]- %b%f%k'

  # and so should everyone else.
  else
    PS1='%F{green}%B<'$USER@$hostname$primes$'>- %b%f%k'
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

#
# General key bindings:
#

# Search for the previous/next line that contains everything from
# beginning to point. So as not to need to ^U^R and retype it.
# See "Prefix searching" on https://zsh.sourceforge.io/Guide/zshguide04.html
bindkey '^xp' history-beginning-search-backward
bindkey '^xn' history-beginning-search-forward

# 2023-07-20: emacs/readline bindings stopped working by default, so force them:
bindkey -e
# bindkey "^P" up-line-or-search

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

# Debian removed highlighting from cal,
# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=904839
# for no good reason (their rationale is that it messes up piping
# to other programs, which makes no sense since highlighting was
# already disabled if it was a tty),
# while leaving the documentation unclear. Get highlighting:
alias cal="ncal -b"

# remindcgi expects its reminders.txt in the same directory as the binary
alias reminders="~/web/cal/index.cgi"

# Run netscheme with sudo -E; it must be root but it also needs access
# to quickbrowse.
alias netscheme='sudo -E /home/akkana/src/netutils/netscheme'

# Newer versions of xterm no longer support titlebar setting with
# the documented sequence of \e]2. But \e]0 works, as long as you
# don't set XTerm*allowSendEvents.
titlebar() {
  # echo ']]2;"$@"'
  echo -e "\033]0; $@ \007"
}

# Copy the primary selection into the clipboard:
alias primary2clip='xsel -p | xsel -i -b'
# and vice versa:
alias clip2primary='xsel -b | xsel -i -p'

# Escape sequence for live font changing in urxvt
# See man 7 urxvt, "Can I switch the fonts at runtime?"
alias bold="printf '\33]50;%s\007' 'xft:Monoisome Tight:size=10:bold:antialias=true'"
alias unbold="printf '\33]50;%s\007' 'xft:Monoisome Tight:size=9:antialias=true'"

# Convert a selection in HTML to plaintext using xclip:
htmlsel2text() {
    if xclip -o -t TARGETS | grep -q text/html ; then
        xclip -o -t text/html | sed 's/<meta [^>]*>//' | xclip -i
        echo "HTML is now on clipboard"
    else
        echo "No text/html on clipboard"
    fi
}

# What's the complement of a number, e.g. the fmask in fstab to get
# a given file mode for vfat files? Sample usage: invert 755
invertmask() {
    python -c "print '0%o' % (0777 - 0$1)"
    # This also works:
    # python -c "print '0%o' % (~(0777 & 0$1) & 0777)"
}

# View in pho every image matching the given fotogr args
phof() {
    imglist=(`fotogr "$@"`)
    if [[ -z $imglist ]]; then
        echo no match
        return
    fi
    pho $imglist
}

# Same thing, but in random order
phofr() {
    imglist=(`fotogr "$@"`)
    if [[ -z $imglist ]]; then
        echo no match
        return
    fi
    pho -R $imglist
}

# View with pho all images tagged photoshare says haven't been shared yet
phoshare() {
    imglist=( $( photoshare -q "$@" | grep -v 'shared on' ) )

    if [[ -z $imglist ]]; then
        echo no match
        return
    fi
    pho -R $imglist
}

# at jobs that can pop up a dialog
atnotify() {
    attime=$1
    shift
    at $attime <<EOF
XAUTHORITY=~/.Xauthority DISPLAY=:0 WINDOWID= zenity --width 700 \
    --title "at $attime" --info \
    --text="<span color='blue' font='15'>at $attime</span>\n\n<span color='red' font='14'>$*</span>"
EOF
}

# Simple pastebin via netcat. Pipe input into this.
alias pastebin='nc termbin.com 9999'

# Make wget use today's date, not the date on the server
alias wget='wget --no-use-server-timestamps'

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

ls() { /bin/ls -FH --color "$@" ; }
ll() {
    /bin/ls -laFH "$@"
    show_symlinks "$@"
}
llt() { /bin/ls -laSHFLt "$@" ; }
llth() { /bin/ls -lFSHLt "$@" | head -20 ; }

# There are lots of ways to list only directories,
# each with its own pitfalls. Here are some attempts:

# This didn't use to work but now does. Go figure.
lsdirs() {
    d="$1"
    if [[ $d == '' ]]; then d=.; fi
    env ls -1FH "$d" | sed -n 's|/$||p' | column;
}

# Next two work except for dirs with spaces in the name:
lsdirs1() {
  (cd $1; /bin/ls -d $(/bin/ls -1F | grep / | sed 's_/$__'))
}

# This used to work but no longer does
lsdirs2() {
  echo `/bin/ls -1F $@ | grep / | sed 's_/$__'`| tr -s ' ' '\n' | paste - - - | column -x -t -c3
}

# pushd, but not if we're already at the target directory
# or if we're currently home.
# Use in other scripts that need to save the previous directory.
pushd_maybe() {
    cwd=`pwd`
    if [[ "$1" == "$cwd" ]]; then
        return
    fi
    if [[ "$cwd" == "$HOME" ]]; then
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

# Click on a window and find which executable is behind it
alias prognamee='basename $(/bin/ls -l /proc/$(xprop _NET_WM_PID | awk "{print \$NF}")/exe | awk "{print \$NF}")'

##################
# Recursive greps
gr() {
  find . -name '*.o' -prune -or -name '*.so' -prune -or -name '*.a' -prune -or -name '*.pyc' -prune -or -name '*.jpg' -prune -or -name '*.JPG' -prune -or -name '*.png' -prune -or -name '*.xcf*' -prune -or -name '*.gmo' -prune -or -name '.intltool*' -prune -or -name '*.po' -prune -or -name 'po' -prune -or -name '*.tar*' -prune -or -name '*.zip' -or -name '.metadata' -or -name 'build' -prune -or -name 'obj-*' -or -name '.git' -prune -or -name '.svn' -prune -or -name '.libs' -prune -or -name __pycache__ -prune -or -type f -print0 | xargs -0 grep "$@" /dev/null
}

zgr() {
  find . -name '*.o' -prune -or -name '*.so' -prune -or -name '*.a' -prune -or -name '*.pyc' -prune -or -name '*.jpg' -prune -or -name '*.JPG' -prune -or -name '*.png' -prune -or -name '*.xcf*' -prune -or -name '*.gmo' -prune -or -name '.intltool*' -prune -or -name '*.po' -prune -or -name 'po' -prune -or -name '*.tar*' -prune -or -name '*.zip' -or -name '.metadata' -or -name 'build' -prune -or -name 'obj-*' -or -name '.git' -prune -or -name '.svn' -prune -or -name '.libs' -prune -or -name __pycache__ -prune -or -type f -print0 | xargs -0 zgrep "$@" /dev/null | fgrep -v .svn | fgrep -v .git
}

# Grep in an android workspace: exclude all the intermediate and
# generated files.
andgr() {
    find . -name 'build' -prune -or -name '*.apk' -prune -or -type f -print0 | xargs -0 zgrep -H "$@"
}

cgr() {
  find . \( -name '*.[CchH]' -or -name '*.cpp' -or -name '*.cc' -or -name '*.ino' \) -print0 | xargs -0 grep -H "$@"
}
hgr() {
  find . \( -name '*.h' -or -name '*.idl' \) -print0 | xargs -0 grep -H "$@"
}
rgr() {
  find . \( -name '*.rb' -or -name '*.rhtml' \) -print0 | xargs -0 grep -H "$@" | fgrep -v .svn
}
htgr() {
  find . \( -name '*.*htm*' -or -name '*.blx' \) -and -not -name 'webhits*' -prune -print0 | xargs -0 grep -H "$@"
}
pygr() {
  find . -name '*.py' -print0 | xargs -0 grep -H "$@"
}
jgr() {
  find . -name '*.js' -print0 | xargs -0 grep -H "$@"
}
xgr() {
  find . \( -name '*.cChH' -or -name '*.cpp' -or -name '*.xul' -or -name '*.html' -or -name '*.js' -or -name '*.css' \) -print0 | xargs -0 grep -H "$@"
}
cssgr() {
  find . -name '*.css' -print0 | xargs -0 grep -H "$@"
}
mgr() {
  find . -name '*akefile*' -print0 | xargs -0 grep -H "$@"
}
agr() {
  find . -type f -print0 | xargs -0 grep -H "$@"
}
javagr() {
  find . -name '*.java' -print0 | xargs -0 grep -H "$@"
}
# zgr() {
#  find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 zgrep -H "$@"
#}
# Next doesn't work. How do we use -prune?
idagr() {
  find . \( -name OBJ -prune -or -name external -prune -or -name '*scons*' -prune -or -name google_appengine -prune -o -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pyc' \) -print0 | xargs -0 grep -H "$@" | fgrep -v .svn | fgrep -v .git
}

# For some reason, with Tags/Keywords this fails with -print0/-0
# but works without it.
taggr() {
  find . -name 'Tags' -or -name 'Keywords' | xargs grep -H "$@"
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
alias flaskfilter="sed 's/.\{12\}\([0-9]\{2\}:[0-9]\{2\}\).\{70\}\(.*$\)/\1 \2/'"
# Apache logs start with a lot of cruft that isn't actually important.
# But keep the date part:
alias flaskfilter="sed 's/.\{12\}\([0-9]\{2\}:[0-9]\{2\}\).*\] /\1 /'"

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

# Distros keeps changing the suspend command; make an alias that won't change:
  # alias zzz='sudo /etc/acpi/sleep.sh'
  # alias zzz="sudo pm-suspend --auto-quirks"
  alias zzz="systemctl suspend"
fi

######################################
# audio/video aliases

movieres() {
    foreach f ($*)
        echo $f :
        ffprobe -v quiet -print_format json -show_format -show_streams $f | egrep '(width|height)'
    end
}

mov2720p() {
    foreach infile ($*)
        outfile=720-$infile:t:r.mkv
        ffmpeg -i $infile -c:v libx264 -preset slow -crf 18 -c:a copy -pix_fmt yuv420p -vf scale=1280:-1 $outfile
    end
}

# From drc on #gimp:
mov2mpeg4() {
    # mencoder has changed its arg structure and this no longer works
    # mencoder $1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg4:vqmin=2:vlelim=-4:vcelim=9:lumi_mask=0.05:dark_mask=0.01:vhq -o $2
    #ffmpeg -i $1 -c copy $1:t:r.mp4
    # but that sometimes produces movies Firefox won't play.
    # This seems to work better:
    ffmpeg -i $1 -vcodec libx264 -acodec aac $1:t:r.mp4
}

# Extract audio from flash:
alias flash2mp3='soundconverter -b -m "audio/mpeg" -s ".mp3"'

# Convert Apple AAC .m4a to mp3:
2mp3() {
    foreach infile ($*)
        outfile=$infile:t:r.mp3
        echo $outfile
        ffmpeg -v 5 -y -i $infile -acodec libmp3lame -ac 2 -ab 192k $outfile
    end
}

# Get resolution of a movie file:
moviesize() {
    for f in "$@"; do
        echo "$f:"
        ffprobe -v quiet -print_format json -show_format -show_streams "$1" | egrep '(width|height)'
    done
}

# Playing DVDs with mplayer. f => fullscreen, v -> no subtitles
alias playdvd="mplayer dvd://1 -alang en"

# camsetting() {
#     whichval="$1"
#     newbright="$2"
#     if [[ "$newbright" == "" ]]; then
#         v4l2-ctl -d /dev/video0 --get-ctrl $whichval
#     else
#         v4l2-ctl -d /dev/video0 --set-ctrl $whichval="$newbright"
#     fi
# }
# alias cambright="camsetting brightness"
# alias camcontrast="camsetting contrast"
# alias camexposure="camsetting exposure_absolute"
# alias camnight="v4l2-ctl -d /dev/video2 --set-ctrl white_balance_automatic=0; v4l2-ctl -d /dev/video2 --set-ctrl white_balance_temperature=2700"

######## end video aliases

######## Prettyprinting

# Prettyprint a JSON file:
ppjson() {
    # This works but reorders the json:
    # python -m json.tool "$@"
    # This doesn't and is just as fast:
    jq . "$@"
}

# Prettyprint HTML. There's also tidy.
pphtml() {
    python3 -c "from bs4 import BeautifulSoup; print(BeautifulSoup(open('$1'), 'lxml').prettify())"
}

# Prettyprint XML:
ppxml() {
     xmllint --format "$@"
}

################################################
# Some GPS conversions

kmz2gpx() {
    # unzip the KMZ, since gpsbabel STILL doesn't know how to do that
    # despite KMZ becoming the most popular format on the net:
    kmlfile=/tmp/$1:t:r.kml
    gunzip -c $1 > $kmlfile
    # :t takes the basename, :r removes the extension
    gpsbabel -i kml -f $kmlfile -o gpx -F $kmlfile:t:r.gpx
}

# Convert a pair of UTM coordinates in NM to a GPX file with one waypoint.
# Usage: utm2gpx 0353315 3954857
# I don't know how to get the UTM zone if you don't already have it;
# Barbara just gives me the pair of points without a zone.
# XXX This doesn't work for utm2gpx 0381100 396373
# which should come out as 35.810504 -106.316062
# compare https://www.engineeringtoolbox.com/utm-latitude-longitude-d_1370.html
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
    printf "         All   New  Re-reads\n"
    for f in ~/Docs/Lists/books/books[0-9](#c4); do
        year=$(echo $f | sed 's/.*books//')
        let allbooks=$(egrep '^[^ ]' $f | grep -v 'Book List:' | wc -l)
        let rereads=$(egrep '^[-.@\*]' $f  | grep -v 'Book List:'| wc -l)
        # How to do numeric computations in zsh:
        printf "%4s:   %3d   %3d   %3d\n" \
               $year $allbooks $(($allbooks - $rereads)) $rereads
    done
}

################################################
# Presentations and monitor switching:

# Enable/disable screen blanking.
# Note: xset -q will show settings.

screenblankoff() {
    xset -dpms
    xset s off
    # If those aren't enough, try adding this
    # xset s 0
}

screenblankon() {
    xset +dpms
    xset s on
}

# Connect to a projector on the VGA port:
alias projector='xrandr --output VGA-1 --mode 1024x768; screenblankoff'

# and on the HDMI port duplicating the laptop screen:
# alias projectorh='xrandr --output HDMI1 --mode 1024x768'
alias projectorh='xrandr --output eDP-1 --auto --output HDMI-1 --mode 1024x768; screenblankoff'

# Projector using the HDMI port, to the right of the laptop display:
# alias projector2='xrandr --auto --output HDMI-1 --mode 1024x768 --right-of eDP-1'
alias projector2='xrandr --output eDP-1 --auto --primary --output HDMI-1 --mode 1024x768 --right-of eDP-1; screenblankoff'

# and set video back to normal:
# alias monitor='xrandr --output HDMI1 --mode 1680x1050 --output VGA1 --off --output LVDS1 --off'
alias noprojector='xrandr --auto; screenblankon'
# See also my checkmonitor script.

# My external monitor designation depends on whether I'm using a dock.
# Could be DP-1 or HDMI-1
extmonitor=HDMI-1

# Configure an external monitor to the right of the current one,
# at the monitor's native resolution
alias 2mon=' xrandr  --output eDP-1 --auto --output $extmonitor --auto --right-of eDP-1'
# alias 2mon='xrandr --auto --output HDMI-1 --auto --above eDP-1'

alias monlaptop='xrandr --output eDP-1 --auto --output $extmonitor --off'
alias monbig='xrandr --output $extmonitor --auto --output eDP-1 --off'

# Use the laptop screen at a lower than normal resolution:
# that way you can screenshare the whole desktop on Zoom without everything
# looking tiny to the audience.
# Keep the external monitor on for other work; Zoom recordings will
# use the screen the Zoom app is on.
# This --right-of stuff doesn't work if you start with the extmonitor active.
# so turn it off first, then back on. The sleep in between may not be needed.
alias monzoomlaptopslides='xrandr --output $extmonitor --off --output eDP-1 --auto; sleep 2; xrandr --output eDP-1 --mode 1024x768 --output $extmonitor --auto --right-of eDP-1'
alias monzoomlaptopdemo='xrandr --output $extmonitor --off --output eDP-1 --auto; sleep 2; xrandr --output eDP-1 --mode 1280x800 --output $extmonitor --auto --right-of eDP-1'

# Toggle mute. This doesn't work when called from an openbox key event,
# but does work from the commandline.
# You may need to run pavucontrol first and disable all but the real output
# since that seems to be the only way to set the default sink.
alias mute="pactl set-sink-mute @DEFAULT_SINK@ toggle"
# End external monitor/audio connections

# For notes during planetarium shows:
# red/black for night vision, narrow to show two at once on a laptop.
alias planeterm="nohup rxvt -geometry 62x45 -fn terminus-iso8859-2-bold-18 -bg black -fg red &"

# Making a PDF from a bunch of slides
alias talk2pdf='qhtmlprint $( fgrep .html slides.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '
alias talk2pdf1024='qhtmlprint -1024 $( fgrep .html slides.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '
alias talk2pdf1366='qhtmlprint -1366 $( fgrep .html slides.js  | grep -v // | sed -e "s/\",/\"/" -e "s/\"//g" ) '

########## End presentation-related aliases

# Photo alias: Delete all .cr2 files that don't have a corresponding .jpg.
# (That way I can manage my jpgs with metapho and anything deleted, I
# can easily delete the corresponding raw file as well.)
# I don't use this much, but it doubles as a reminder of how to do
# fancy pattern subs in zsh scripts.
# Assume files are in the current directory.
delcr2() {
    echo Removing *.cr2(e:'[[ ! -e ${REPLY%.cr2}.jpg ]]':)
    sleep 3
    rm *.cr2(e:'[[ ! -e ${REPLY%.cr2}.jpg ]]':)
}

################################################
# Mount, df and other filesystem-related stuff,
# to get rid of all the crap they show now.

# Mount and df no longer suffice to show mounted filesystems,
# since they show so much irrelevant virtual filesystem crap now.
# Here are ways to clean them up:
mount() {
    if [[ $# -ne 0 ]]; then
        /bin/mount "$@"
        return
    fi

    # Else called with no arguments: we want to list mounted filesystems.
    /bin/mount -t nosysfs,nodevtmpfs,nocgroup,nomqueue,notmpfs,noproc,nopstore,nohugetlbfs,nodebugfs,nodevpts,noautofs,nosecurityfs,nofusectl,nosquashfs,nocgroup2,noefivarfs,nobpf,noconfigfs,nofuse.gvfsd-fuse

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
    /bin/df -hTx tmpfs -x devtmpfs -x rootfs -x squashfs
}

# Mount a USB stick or SD card.
# On the CX1, there's no predicting when a USB device will show up on
# sda1 or sdc1. I get tired of needing to run lsblk and sudo every time
# and I never remember all the mount options.
musb() {
    # zsh arrays
    devices=(/dev/sda1 /dev/sdc1 /dev/sdd1 /dev/sde1)
    foreach device ($devices)
        echo Trying $device
        # -o umask=111,uid=myuid is supposed to work, but doesn't.
        # This gives all permissions to all users::
        sudo mount -o rw,umask=111,dmask=000,shortname=lower $device /pix
        # sudo mount -o uid=$uid,umask=111,dmask=000,shortname=lower $device /pix
        if [ $? -eq 0 ]; then
            echo "mounted $device on /pix"
            return
        fi
    end

    echo "Couldn't find anything to mount in" $devices
}

# Upload movies from the Sony a6100's obscure locations on the SD card:
sonymovies() {
    if [[ -d $1 ]]; then
        dest=$1
    else
        dest=.
    fi
    mv /sony/private/m4root/clip/*  $dest
    # Remove silly thumbnails
    rm /sony/private/m4root/thmbnl/*
}
sonyall() {
    if [[ -d $1 ]]; then
        dest=$1
    else
        dest=.
    fi
    mv /sony/dcim/100msdcf/* $dest
    sonymovies $dest
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

# For the Raspberry Pi, the serial port connections are
# 6=black, 8=white, 10=green
alias rpi='titlebar "Raspberry Pi"; echo "black=Gnd white=TX green=RX"; echo "Disconnect with Ctrl-backquote d"; screen /dev/ttyUSB0 115200; titlebar "local"'

# Fritzing as of 0.9.3 insists on creating ~/Documents and won't let
# that be overridden. The only workaround I've found is changing HOME.
# Also, the Ubuntu version of Fritzing is a script that doesn't pass
# command-line arguments, so duplicate the script here to allow args.
fritzing() {
    cd /usr/share/fritzing/parts
    HOME=/home/akkana/Fritzing /usr/bin/fritzing.real $@ &
}

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
    # doesn't work:
    # allnet=$(mynet | sed 's_\.[0-9]*/24_.1-254_')
    # nmap $allnet -p$1 --open -oG - | grep $1/open

    # better
    nmap -p $1 --open $(mynet | sed 's_255$_0/24_')
}

# A few electronics cheatsheets:
alias gpio-pi='pho -P ~/src/pi-zero-w-book/images/raspi-gpio.jpg &'
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

HISTFILE=~/.config/zsh/histfile
# This was set at 1000, but temporarily let's boost it
# to do some shell history analysis.
HISTSIZE=10000
SAVEHIST=$HISTSIZE

# Someone suggested this to avoid losing history on poweroff, etc.
setopt inc_append_history

# History from past days doesn't include timestamps unless you set this:
setopt EXTENDED_HISTORY

setopt appendhistory notify
unsetopt autocd

# This is apparently Ubuntu-specific weirdness:
#skip_global_compinit=1

# The following lines were added by compinstall
#zstyle :compinstall filename '/home/akkana/.zshrc'

# End of lines added by compinstall

# When completing/history saving a | or &, don't eat the space before it.
ZLE_REMOVE_SUFFIX_CHARS=$' \t\n;'

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

# Don't complete parameters in the command position
# IE don't complete termc<TAB> to "termcap="
# '! parameters' - in tag-order tells compsys to ignore that tag, but use the others
zstyle ':completion::complete:-command-::' tag-order '! parameters' -

# Some tuts on writing custom completions:
# http://askql.wordpress.com/2011/01/11/zsh-writing-own-completion/
# https://web.archive.org/web/20190411104837/http://www.linux-mag.com/id/1106/

# Much more verbose info (while learning/debugging compdefs).
zstyle ":completion:*" verbose yes
zstyle ":completion:*:descriptions" format '%B%d%b'
zstyle ":completion:*:messages" format '%d'
zstyle ":completion:*:warnings" format 'No matches for: %d'
zstyle ":completion:*" group-name

# _mutt uses _mailboxes, which figures the default mail folder is ~/Msgs
# but allows changing it:
set mail-directory='~/Msgs'

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

  # XXX TEMPORARILY DISABLING ALL THESE IN CASE THEY'VE BEEN FIXED

  # Recently autocompletion for cp is broken. Override it:
  # compdef _files cp

  # By default (no CLASSPATH SET), autocompletion for java searches
  # recursively starting from .  Don't try it in your homedir!
  # Not sure if this really turns it off, though -- had a typo.
  # #compdef _files java

  # loadkeys also has "smart" (* un-smart) completion.
  # compdef _files loadkeys

  # adb hangs trying to autocomplete anything -- apparently it's
  # actually trying to talk to the android even when you're trying
  # to autocomplete a local filename.
  # The weird thing is, with this line uncommented, you can autocomplete
  # adb she<tab> but with it commented out, you can't -- seems backward!
  # compdef _files adb

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
# (But in 2023, even with compinit it doesn't do anything.
# https://zsh.sourceforge.io/Guide/zshguide06.html says that
# it's bound by default to ^Xh', but that doesn't do anything either.)

#bindkey '\e\d' _complete_help
bindkey '\e\e' _complete_help

# zsh sometimes replaces a command with the completions when you hit tab.
# But you can undo that and go back to what you were typing.
# Normally it's ^X^U but this is easier to remember:
bindkey '^Z' undo
bindkey '^X^R' redo

# Two other useful bindings suggested by Mikachu:
# the former lets you press a key and see what's bound to it,
# the latter finds all keys that are bound to the specified widget
# since you can also tabcomplete, you can also usually find
# something useful by typing a prefix and tabbing
bindkey '^X^D' describe-key-briefly
bindkey '^X^W' where-is

######## end zsh completion #######################
######## end zsh-specific options #################

################################################
# Build/development helpers

# export GIMP_PREFIX=$HOME/run/gimp-master

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
    meson -Dprefix=$HOME/run/hexchat build
    ninja -C build
    ninja -C build install

    popd_maybe
}

#############################################################
# Debian apt helpers.


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
# Python

if [[ -f ~/.config/pythonrc ]]; then
    export PYTHONSTARTUP=~/.config/pythonrc
# else
#     echo "~/.config/pythonrc doesn't exist, no PYTHONSTARTUP"
fi

######
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
# python3 -m venv --system-site-packages ~/pythonenv/3env   (python3)
#   (requires python3-venv)
# virtualenv --system-site-packages ~/pythonenv/2env        (python2)
#   (requires virtualenv and python-virtualenv)
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
    vers=$1

    nopythonenv

    env2=$HOME/pythonenv/2env
    env3=$HOME/pythonenv/3env

    # if [[ $arch == 'x86_64' ]]; then
    #     archbits=64
    # else
    #     archbits=32
    # fi

    if [[ $vers == 23 ]]; then
        echo "Using both Python 2 and 3 envs (experimental)"
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $env2/bin/activate
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $env3/bin/activate
    elif [[ $vers == 2 ]]; then
        echo Switching to Python2 env
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $env2/bin/activate
    elif [[ $vers == 3 ]]; then
        echo Switching to Python3 env
        VIRTUAL_ENV_DISABLE_PROMPT=1 source $env3/bin/activate
    else
        echo Version should be 2, 3 or 23
        unset vers
        return
    fi

    set_prompt
    PS1="%F{magenta}Py${vers} ${PS1}"
    unset vers
}

alias python2env='switchpythonenv 2'
alias python3env='switchpythonenv 3'
alias python23env='switchpythonenv 23'

# Create a new temporary env for testing something
newpythonenv() {
    nopythonenv

    newenv=$(mktemp -d /tmp/pyenvXXX)
    echo "Making new virtualenv in $newenv"
    python3 -m venv --system-site-packages $newenv
    . $newenv/bin/activate
    titlebar "Python $newenv"
}

# Enble python3env by default, no special prompt.
# Doing this from .zlogin or before starting X isn't recommended;
# it makes it impossible to deactivate it since deactivate is
# only defined in the local shell, not in environment variables.
# But do it in all interactive shells under X.
if [[ $(tty) =~ '/dev/pts/.*' && -e $HOME/pythonenv/3env/bin/activate ]]; then
    VIRTUAL_ENV_DISABLE_PROMPT=1 source $HOME/pythonenv/3env/bin/activate
fi

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

# Commented out because accidentally typing this in billtracker
# can stomp the local database.
# alias unittest='python3 -m unittest discover'

# Run unit tests under cProfile
alias unitprofile='python3 -mcProfile -m unittest discover'

#############################################################
# Android-related aliases

# Some aliases for getting files from Android via adb,
# since the lack of usb-storage and autocomplete is such a pain.

# Where is the SD card on my phone?
# Under KitKat, it's at /storage/extSdCard.
# Under Marshmallow, it's at /storage/nnnn-nnnn
# Set androidSD in .zshrc.hostname: these aliases require it.
# On Google devices, there's no SD card so just use /storage/emulated/0.
androidSD=/storage/emulated/0

pulladb() {
    # Pull files from a particular directory on an android device,
    # then optionally remove the files thus transfered.
    # Usage: pulladb from-dir-on-device [-k]
    # -k means keep: don't delete afterward
    pullfromdir=$1
    flags=$2

    # adb always fails the first few times, timing out while connecting
    adb shell echo initializing adb ...
    sleep 1
    adb shell echo initialized adb

    adb pull $pullfromdir/. .

    # This always pulls an empty dir named thumbnails.
    # Remove it in case it's ever not empty.
    rm -rf thumbnails

    if [[ "$flags" == '-k' ]]; then
        echo "Keep mode: NOT deleting photos on phone"
    else
        # Find all files under this dir changed in the last 5 minutes
        # Note, man find says -mmin n matches files modified in the last n min,
        # but it's actually -mmin -n.
        pulledfiles=$( find . -type f -mmin -5 -print0 )

        # With xargs -n 1 This prints "rm: Needs 1 argument (see "rm --help")"
        # but seems to work anyway. But without -n 1 removes them all in one
        # command, which should be better anyway.
        echo $pulledfiles | sed "s_\./_$pullfromdir/_g" | xargs -0 adb shell rm
    fi

    echo "========"
    echo "Pulled:"
    ls
}

# Pull .fit files exported from the Specialized bike app
# I leave them in /storage/emulated/0 because I don't want to be bothered
# choosing a directory every time when I'm just going to download & delete.
pull-specialized-fit() {
    adbdir=/storage/emulated/0
    for f in $(adb shell "ls $adbdir/Download/*.fit"); do    # get absolute paths
        adb pull $f
        adb shell rm $f
    done
}

pullgpx() {
    # Pull recorded GPX tracks from OsmAnd.
    pushd_maybe ~/Docs/gps/new

    # If osmand is on an SD card:
    # pulladb $androidSD/Android/data/net.osmand.plus/files/tracks/rec
    # If on main storage:
    pulladb /storage/emulated/0/Android/data/net.osmand.plus/files/tracks/rec $*

    # Also pull any .fit files exported with FX file manager
    pull-specialized-fit
}

pullphotos() {
    # If you keep photos on an SD card, use $androidSD/DCIM/Camera
    # If they're on main storage:
    pulladb /storage/emulated/0/DCIM/Camera $*

    # Deleting photos on android just renames them to .trashed-*
    # Remove the local copy (having just removed the copy on the device).
    # This gives an annoying "No match" error.
    # rm -f .trashed*
    # { rm .trashed*; } 2>/dev/null
    # This is an obscure zsh incantation that avoids the error:
    rm -rf .trashed*(N)
}

pullscreenshots() {
    pulladb /storage/emulated/0/Pictures/Screenshots $*
}

# pullvoice() {
#     pulladb /storage/emulated/0/SoundRecorder $*
# }

# But what if adb isn't installed? Here's how to do it using gphoto2.
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

############## For building Android apps:

# Set up needed environment variables for Android.
androidbuild() {
    ANDROID_BUILD_HOME=/ssd/Android

    export ANDROID_HOME=$ANDROID_BUILD_HOME/android-sdk-linux
    export ANDROID_SDK=$ANDROID_HOME
    # export ANDROID_NDK=$HOME/outsrc/android-ndk-r10d
    export JAVA_HOME=$ANDROID_BUILD_HOME/android-studio/jre/
    # export PATH=$PATH:$ANDROID_BUILD_HOME/android-studio/bin:$ANDROID_BUILD_HOME/gradle/gradle-6.7.1/bin:$ANDROID_BUILD_HOME/android-sdk-linux/tools:$ANDROID_BUILD_HOME/android-sdk-linux/tools/bin:$ANDROID_BUILD_HOME/android-studio/jre/bin

    # Put platform-tools at the beginning, so we get the adb that goes with
    # the other tools. The adb from Ubuntu can't talk to the emulator.
    PATH=$ANDROID_BUILD_HOME/android-sdk-linux/platform-tools:$PATH

    # this must come before android-sdk-linux/tools for some reason.
    # There's an emulator executable in android-sdk-linux/tools,
    # but that one errors out with
    #    PANIC: Missing emulator engine program for 'x86' CPU:
    # It's actually here, so make sure this comes before tools.
    PATH=$PATH:$ANDROID_SDK/emulator

    # Can't find a nicer multi-line syntax for adding to PATH.
    PATH=$PATH:$ANDROID_BUILD_HOME/android-studio/bin
    PATH=$PATH:$ANDROID_BUILD_HOME/gradle/gradle-6.7.1/bin

    PATH=$PATH:$ANDROID_BUILD_HOME/android-sdk-linux/tools
    PATH=$PATH:$ANDROID_BUILD_HOME/android-sdk-linux/tools/bin
    PATH=$PATH:$ANDROID_BUILD_HOME/android-studio/jre/bin

    export PATH

    alias emu="emulator @Pixel_3a_API_30_x86"

    echo "Useful commands:"
    echo "gradlew assembleDebug, gradlew assembleRelease"
    echo "emulator @Pixel_3a_API_30_x86"
    echo "adb install -r app/build/outputs/apk/release/app-release.apk"
    echo "adb shell am start -n com.shallowsky.feedviewer/.MainActivity"
}

# End Android


# Now that I'm running feeds on shallowsky.com,
# local/xtra urls have to be saved there too,
# so this alias appends the given URL to the file there.
# Run with e.g. localurl 'http://blahblah'
# The single quotes are only needed if the URL has an embedded newline,
# like a long URL pasted from mutt or from email from an Apple user.
# Removing the newlines isn't needed with modern zsh that allows
# multiline pastes, but it doesn't hurt anything to keep it.
# Note: in zsh this is no longer needed since pasting newlines
# no longer executes the line immediately and you can edit them out.
remove_newlines() {
    echo ${1/$'\n'/}
}

localurl() {
    ( for url in $* ; remove_newlines $url ) | ssh shallow 'cat >> web/feedme/feeds/localurls'
}


##################################
# Spam and email-related aliases

# Tail the procmail log file, for when I'm expecting mail:
alias proctail="tail -1000f ~/Procmail/log | egrep -v '^procmail'"

# Spast checks spam with e.g. echo $subj | grep -i -f $patfile
# How do we find out from $subj which line in $patfile matched the grep?
# Sample Usage: whichspam 'subject-line' subjectRejects
whichspam() {
    # to print each line before executing, for debugging purposes:
    # set -o xtrace
    subj="$1"
    dir=~/"Procmail/spast"
    whichfile="$2"
    if [[ -z "$whichfile" ]]; then
        whichfile=subjectRejects
    fi
    printf '%s "%s"\n' "Searching in ~/Procmail/spast/$whichfile for" "$subj"
    whichfile="$dir/$whichfile"
    lineno=1
    while read -r line;do
        if grep -E -i -q -- "$line" <<<"$subj"
        then
            printf 'line %d: %s\n' "$lineno" "$line"
            # return # comment this if you want all  matching lines
        fi
        ((lineno++))
    done < "$whichfile"
    #set +o xtrace
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
# Argument is header, e.g. Subject:
spamcheck() {
    header=$1
    shift
    args=$*
    echo looking for $args in $header
    echo "============ Recent =============="
    decodemail -a "$header" ~/Spam/saved | egrep -a -i "$args"
    if [ -d ~/Spam/oldheaders ]; then
        echo
        echo "============ Older =============="
        decodemail -a "$header" ~/Spam/oldheaders/saved | egrep -a -i "$args"
    fi
}

alias spams='spamcheck Subject:'
alias spamf='spamcheck From:'

cleanspam() {
    # Spam is saved in ~/Spam. (Outside my regular mail hierarchy,
    # so it doesn't get synced to my laptop or backed up in minibackups.)
    # Older batches have From, Subject, To headers saved in ~/Spam/oldheaders.
    # Periodically, we need to clean out the current spam folders
    # but save the old headers (not message bodies) for spam filter
    # development purposes.
    mkdir -p $HOME/Spam/oldheaders
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
    if [[ -f $HOME/Procmail/log ]]; then
      tail -7000 $HOME/Procmail/log >$HOME/Procmail/olog
      rm -f $HOME/Procmail/log
    fi
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
alias blogsync='rsync -av --delete ~/web/blog ~/web/blogfiles shallow:web/'

# End PyBlosxom helpers.

####################################################################
# Backups, and syncing with web servers

#
# Usage: fullbackup target, e.g. fullbackup /backupdisk/username/
#
fullbackup() {
    dobackup "$1" full
}

minibackup() {
    dobackup "$1" mini
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

alias remindme='remind -g ~/web/cal/remind.txt | sed -e "s/||/\n/g" -e "s/\bon //"'

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

##### printers ################

# Nice overview of lp options: https://www.computerhope.com/unix/ulp.htm

# Which printers are available? lpstat -p -d also works.
alias whichprinters='lpstat -a; echo "print with lp -d dest -n num-copies"; echo "for duplex,  -o sides=two-sided-long-edge -o collate=true -d dest"; echo "For PDF consider adding -o fit-to-page or -o scaling=100, or using pdfpages or pdfjam"'

alias vboxdrv="sudo modprobe vboxdrv"

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

# http://linuxmafia.com/pipermail/conspire/2021-February/011464.html
tzall () {
    grepfor="$1"
    find /usr/share/zoneinfo/* -type f -print \
        | sed -r 's_/usr/share/zoneinfo/__' \
        | grep -v -e .tab -e posixrules
}

# Also be aware of zdump, e.g. zdump America/{Los_Angeles,Phoenix,Denver,Chicago,New_York} Australia/Sydney Europe/{London,Berlin}
# and, to check a date somewhere else (the 2 levels of quotes are needed):
# date -d 'TZ="Europe/Berlin" 2021-09-17 7pm'

# Tell aptitude not to limit descriptions to the terminal width
alias aptitude='/usr/bin/aptitude --disable-columns'

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

# Always run sqlite inside rlwrap to get better commandline editing.
# This used to be wonderful, but now it messes up the input line.
# alias sqlite3="rlwrap -a -z pipeto -i /usr/bin/sqlite3"

# Which numbers correspond to which colors on this terminal?
alias tcolors='printf "\e[%dm%d dark\e[0m  \e[%d;1m%d bold\e[0m\n" {30..37}{,,,}'

# I can never remember nmap arguments
alias portscan="nmap -v -sT localhost"

# Cute command-line metronome using sox play
# Usage: metronome bpm [minutes [volume]]
# -c1  one channel
metronome() {
    bpm=$1
    if [[ $bpm == '-h' || $bpm == '--help' || $bpm == '' ]]; then
        echo 'Usage: metronome bpm [minutes [volume]]'
        return
    fi
    minutes=$2
    volume=$3
    if [[ $minutes != '' ]]; then
        beats=$(awk "BEGIN { print $bpm * $minutes }")
    else
        beats='-'
    fi
    if [[ $volume == '' ]]; then
        volume=1
    fi
    echo "BPM $bpm minutes $minutes Volume $volume"
    play -n -c1 synth 0.004 sine 2000 pad $(awk "BEGIN { print 60/$bpm -.004 }") repeat $beats vol $volume
}

# Change rxvt foreground and background color. Usage: urxvtcolors bg [fg]
# Color names are rgb.txt colors in lowercase with spaces removed, e.g. lightblue
urxvtcolors() {
    bg=$1
    fg=$2
    if [[ $fg == '' ]]; then
        fg='black'
    fi
    printf "\033]11;$bg\007"
    printf "\033]10;$fg\007"
}

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

