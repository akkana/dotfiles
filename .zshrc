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

# Source global definitions
if [[ -f /etc/zshrc ]]; then
	. /etc/zshrc
fi

#setopt ignoreeof

setopt RM_STAR_SILENT

# Allow completions like *vol*<tab>
setopt globcomplete

# zsh docs say it should be colon separated, but that doesn't work.
# Or sometimes it does -- only on the laptop, for some reason,
# when I'm not reading mail (maybe it's affected by mutt changing
# the atime?) and it's super annoying there.
#mailpath=($HOME/Msgs/in/Inbox $HOME/Msgs/in/whitelist)
#MAIL=(0 $HOME/Msgs/in/Inbox $HOME/Msgs/in/whitelist)

# Prevent any repeated entries in $PATH
typeset -U PATH

# Set path
export PATH=$HOME/bin:$HOME/bin/linux:/usr/local/bin:/usr/sbin:/usr/bin:/bin:/usr/bin/X11:.:/sbin:/usr/games:$HOME/android/android-sdk-linux_x86/tools:$HOME/android/android-sdk-linux_x86/platform-tools:/usr/local/bin

# Need to add android paths for both user AND root in order to use adb.
# SO annoying!

ulimit -c unlimited
HISTSIZE=200

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

# Print time on the right
#RPS1='%~%w %t'
RPS1="%F{red}%~%t%f%k"
# Cool happy/sad face right prompt from saz, from a friend of hers:
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

# aliases

alias sss="titlebar D N A; ssh shallowsky.com; titlebar local"
alias ssm="titlebar Moooooooooon; ssh moon; titlebar local"
alias ssp="titlebar Raspberry Pi; ssh pi@pi; titlebar local"

#ls() { /bin/ls -F --color $* ; }
#ll() { /bin/ls -laF --color $* ; }
#llt() { /bin/ls -laSFHLt --color $* ; }
#llth() { /bin/ls -lFSHLt --color $* | head -20 ; }
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
alias m=mutt
alias pd=pushd
alias s=suspend
alias rl="telnet -r"

alias beep="echo "
alias akk="aplay $HOME/.xchat2/sounds/akk.wav"
alias ap="man -k"

alias screenshot="scrot -b -s screenshot.jpg"
alias thes="dict -h localhost -d moby-thesaurus"

# alias newbg='hsetroot -fill `find -L $HOME/Backgrounds -name "*.*" | randomline`'

alias akk='play ~/.xchat2/sounds/akk.wav'

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
# Next doesn't work. How do we use -prune?
idagr() {
  find . \( -name OBJ -prune -or -name external -prune -or -name '*scons*' -prune -or -name google_appengine -prune -o -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pyc' \) -print0 | xargs -0 grep $* /dev/null | fgrep -v .svn | fgrep -v .git
}
alias igr=idagr

# Search for spam subjects or from lines in Spam/saved,
# for purposes of telling which patterns should be added to procmail filters.
spams() {
    #grep Subject ~/Spam/saved ~/Spam/trained/saved | egrep -i "$*"
    decodemail -a Subject: ~/Spam/trained/saved ~/Spam/saved | egrep -a -i "$*"
}
spamf() {
    #grep -a -h '^From:' ~/Spam/trained/saved ~/Spam/saved | egrep -a -i "$*"
    decodemail -a From: ~/Spam/saved ~/Spam/trained/saved | egrep -a -i "$*"
}
spamff() {
    #grep -a -h '^From' ~/Spam/trained/saved ~/Spam/saved | egrep -a -i "$*"
    decodemail -a From ~/Spam/saved ~/Spam/trained/saved | egrep -a -i "$*"
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

# blog stuff -- helpers for pyblosxom
blogupdate() {
  pushd ~/web/blogfiles
  setopt localoptions errreturn
  pyblosxom-cmd staticrender --incremental
  ~/bin/blogtopics
  mv ../blog/topics.html ../blog/oldtopics.html
  mv ../blog/newtopics.html ../blog/topics.html
  blog-tag-index
  popd
}

blogup() {
  pushd ~/web/blogfiles
  setopt localoptions errreturn
  pyblosxom-cmd staticrender --incremental
  popd
}

# Sync new blog files back to the server:
#alias blogsync='rsync -av ~/web/blog ~/web/blogfiles leewit:shallow/'
alias blogsync='rsync -av ~/web/blog ~/web/blogfiles shallowsky.com:web/'

# Palm stuff: USB0 for Tungsten, USB1 for everything else
export PILOTPORT=/dev/ttyUSB1

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
mov2mp3() {
  avconv -i $1 $2
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
#alias temp="cat /proc/acpi/thermal_zone/*/temperature"
alias temp=sensors

# Record a realaudio stream
getreal() {
  mplayer -playlist $1 -ao pcm:file=$2 -vc dummy -vo null
}
# Then transcode it with: 
# lame --tg Other --ta artist -tl album file.wav file.mp3

# Ratings for my book
alias ratings='links -dump "http://www.amazon.com/gp/product/1590595874" | grep "in Books" | grep -v Explore'

# For presentations
alias bigterm="rxvt -geometry 80x33 -fn '-*-lucidatypewriter-*-*-*-*-19-*-*-*-*-*-*-*'"
alias noteterm="xterm -geometry 30x34+1025+0 -fn '-*-terminus-bold-*-*-*-22-*-*-*-*-*-*-*'"

# Some whizzy commands you can set to do various things,
# only most of them don't seem to work reliably:

function prompt_command {
  echo command was $_, result was $?
  echo foo bar $*
}

function command_not_found_handle {
  filename=$1
  echo processing error on $1
  echo Command not found: $filename
  if [[ -e $filename ]]; then
    if [[ ! -x $filename ]]; then
      if [[ ! -r $filename ]]; then
        echo "$filename is a file but it's not readable or executable."
        echo "Maybe you need to be root?"
        return 127
      fi
      echo "$filename is a file. Did you mean to view it:"
      echo "  cat $filename"
      echo "  less $filename"
      echo "or edit it:"
      if [[ x"$VISUAL" != x ]]; then
        echo "  $VISUAL $filename"
      elif [[ x"$EDITOR" != x ]]; then
        echo "  $EDITOR $filename"
      else
        echo "  vim $filename"
      fi
    fi
  fi
  echo "$*"
  return 127
}

not_accept_command() {
  #test -f "$BUFFER"
  echo accept_command
  return 0
}

#zle -N accept-line accept_command

#TRAPZERR=command_not_found_handle
#ACCEPT_LINE=accept_command
#PROMPT_COMMAND=prompt_command

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
  # zsh has some kind of smart git completion that doesn't autocomplete file
  # or directory names. I ask you, how smart is that?
  # But blah! compdef doesn't exist in the zsh in Debian squeeze.
  # I hope they don't have the smart completion either.
  #compdef _files git

  # By default (no CLASSPATH SET), autocompletion for java searches
  # recursively starting from .  Don't try it in your homedir!
  # Not sure if this really turns it off, though -- had a typo.
  #compdef _files java

  # loadkeys also has "smart" (* un-smart) completion.
  compdef _files loadkeys

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
###################################################

#alias zzz='sudo /etc/acpi/sleep.sh'
alias zzz='sudo pm-suspend --auto-quirks'

#alias netscheme='sudo /etc/network/schemes/netscheme'
alias netscheme='sudo /home/akkana/bin/netscheme'

if [ -f ~/ideascopic/dev/tools/config/setup_dev_env.bash ]; then
    source ~/ideascopic/dev/tools/config/setup_dev_env.bash
fi

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
alias rpi='screen /dev/ttyUSB0 115200'

alias beagleroute='sudo iptables -A POSTROUTING -t nat -j MASQUERADE; echo 1 | sudo tee /proc/sys/net/ipv4/ip_forward > /dev/null'

# Connect/disconnect from a docking station. Obsoleted by shell script.
#alias dock='xrandr --output VGA1 --mode 1600x900; hsetroot -center `find -L $HOME/Backgrounds -name "*.*" | randomline`; xrandr --output LVDS1 --off'
#alias undock='xrandr --output LVDS1 --mode 1280x800; hsetroot -center `find -L $HOME/Backgrounds -name "*.*" | randomline`'

alias vpn='pushd ~/vpn/vpn-credentials-akkana; sudo cp /etc/network/schemes/resolv.conf-ida /etc/resolv.conf; sudo openvpn ideascopic.conf &; popd'

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

# Older zsh, like on squeeze, don't have compdef.

# zsh annoyingly only prints the last 10 lines of history by default.
alias history='history 200'

# Connect to a projector on the VGA port:
alias projector='xrandr --output VGA1 --mode 1024x768'
alias noprojector='xrandr --auto'

# Feedme helpers:
# Mount the Android device, copy its list of URLs, run feedme
# then back up everything:

minifeed() {
  if ! feedme 'A Word A Day' 'Jon Carroll' 'Slate' 'Language Log' 'Merc local news' 'Xtra'
  then
    echo "Error running feedme"
    return
  fi

  curdate=`date +%m-%d-%a`
  echo "You need to: cp -r ~/feeds/$curdate /droidsd/feeds/"
}

feed() {
  setopt localoptions noerrreturn
  mount /droidsd
  if ! mount | grep droidsd > /dev/null 2>&1
  then
    echo "Couldn't mount /droidsd -- exiting"
    return
  fi

  # From here on, return from the function if anything goes wrong
  setopt localoptions errreturn
  urlrss

  if ! feedme
  then
    echo "Error running feedme"
    return
  fi

  curdate=`date +%m-%d-%a`
  echo "Copying $curdate to /droidsd/feeds/"
  cp -r ~/feeds/$curdate /droidsd/feeds/
  sync

  # This actually might not be a good idea -- we'd want to --delete,
  # but then what if something's wrong with the mounted microSD?
  #rsync -av /droidsd/feeds/ ~/droidfeeds/

  # Comment out for the Archos -- seems to encourage filesystem errors.
  # Hopefully the Samsung will be more reliable.
  umount /droidsd &

  # Syncing back to the server is optional,
  # but we want to list the new feeds whether or not syncing succeeds:
  setopt localoptions noerrreturn
  echo "Syncing back to the server"
  rsync -av ~/.cache/feedme/ shallowsky.com:.cache/feedme/

  echo "New feeds:"
  /bin/ls ~/feeds/$curdate
}

alias syncfeeds='rsync -av ~/.cache/feedme/ shallowsky.com:.cache/feedme/'

# Now that we're running feeds on shallowsky.com,
# local/xtra urls have to be saved there too.
# Run with e.g. localurl http://blahblah
remove_newlines() {
    # #" expands escape sequences like \n
    echo ${1/$'\n'/}
}
localurl() {
    ( for url in $* ; remove_newlines $url ) | ssh shallowsky.com 'cat >> web/feedme/feeds/localurls'
}

# Remove podcast files except recent ones:
# Fancy zsh method, but I must have a typo in it 'cause it doesn't work:
#alias prunepod='rm $HOME/POD/**/*(.mw+2)'
alias prunepod='find ~/POD -type f -ctime +20 -exec rm "{}" \;'

# Copy podcasts to the mp3 device
#    echo cp $(cat $filename) "/media/mobile disk/"
pods() {
  filename=$(ls -1t `find ~/POD -name '*.m3u'` | head -1)
  echo filename $filename
  if [[ x$filename == x ]]; then
    echo "Sorry, couldn't find most recent podcasts"
  else
    echo "Copying files from $filename"
    pushd ~/POD
    # Prepend the date to every filename, so an mp3 player that
    # orders by date will play podcasts in date order.
    date=$(date '+%y-%m-%d')
    # Can't store filenames in a variable, or foreach will treat them
    # as a long string with newlines in it, instead of separate items.
    #files=$(cat $filename)
    foreach fil in $(cat $filename)
        f=$(basename $fil)
        echo "  $f"
        cp "$fil" "/mp3/$date-$f"
        #cp "$fil" "~/pods/$date-$f"
    end
    popd

    ls -t /mp3
  fi
}

# If you're working on a branch, and all your changes are committed,
# use this to merge master changes into the current branch.
alias git_merge_branch='git fetch; git rebase origin/master'

function ec2forward()
{
  ssh -L 5984:localhost:5984 ida@partners.ideascopic.com
}

# Pipe a function through a JSON prettyprinter, omitting any Content-type arg.
# Sadly, mjson.tool doesn't work for dicts the way Python prints them, with
# single quotes; they must be mapped to double-quotes to work.
# Usage: ppjson myscript.cgi
function ppjson()
{
    $* | grep -v Content-type | tr "'" '"' | python -mjson.tool
}

# Note to self about what args I need for unison to preserve times
# and not prompt on every file:
# unison -force source -times -auto source dest
# Reference guide:
# http://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html#tutorial

alias idamail='mutt -F ~/.mutt/ideascopic'
alias shallowimap='mutt -F ~/.mutt/shallowimap'
alias patmail='mutt -F ~/.mutt/patimap'
alias znet='mutt -F ~/.mutt/znet'
alias moonmail='mutt -F ~/.mutt/moon'

# Figure out whether we're home or away.
# if ping -c 1 -q -W 1 moon ; then
#     echo "We're at home"
#     alias m='mutt -F ~/.mutt/moon'
# else
#     echo "We're traveling"
#     alias m=mutt
# fi
# But we're not doing that right now:
alias m=mutt

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

# Controlling printers: lpstat -a will list queue names
alias lpbrother='lp -d Brother_HP_LaserJet_4050_Series'

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

nextper() {
    d=$( grep H: ~/Docs/Lists/health | grep -w P | tail -1 | cut -b -10 )
    echo Last: $d
    # d is format yyyy-mm-dd
    echo -n 'Next: '
    # date --date=$d'+28 days' +'%Y-%m-%d'
    date --date=$d'+28 days' +'%b %d'
}

# Tell aptitude not to limit descriptions to the terminal width
alias aptitude='aptitude --disable-columns'

# Adjust for day or nighttime monitor modes
alias day="xrandr --output HDMI1 --brightness 1.0"
alias night="xrandr --output HDMI1 --brightness .8"

alias kindle="wine ~/.wine/drive_c/Program\ Files/Amazon/Kindle/Kindle.exe"
alias adobeDE="wine ~/.wine/drive_c/Program\ Files/Adobe/Adobe\ Digital\ Editions/digitaleditions.exe"

# R has no way to tell it not to prompt annoyingly to save the environment
# every time you quit, except as a commandline flag:
alias R="/usr/bin/R --no-save"

# Convert temperatures between F and C, because units' stupid syntax
# is impossible to remember.
c2f() {
    units "tempC($1)" tempF
}
f2c() {
    units "tempF($1)" tempC
}

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
    echo "$1" | egrep -i -- "$line"
    if [[ $? == 0 ]]; then
      echo $line
    fi
  done
  #set +o xtrace
}

# Linux has a lovely list of all compose key sequences.
composekey() {
  grep $1 /usr/share/X11/locale/en_US.UTF-8/Compose
}

# Display a text calendar some number of months (default 2)
# using my remind database:
mycal() {
    months=$1
    if [[ x$months == x ]]; then
        months=2
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
    fullexcludes=( Cache .cache/mozilla Spam LOG log olog Tarballs \
        VaioWin Bitlbee core outsrc .imap POD .cache/openbox/openbox.log \
        .icons .thumbnails .cache/thumbnails .imap .macromedia core .histfile \
        webapps store.json.mozlz4 .dbus/session-bus .emacs-saves )

    # Exclude these from "mini-full" backups (e.g. if low on backup disk space)
    moreexcludes=( '*.mp4' '*.img' '*.iso' DVD \
        .config/chromium .cache/chromium outsrc .VirtualBox 'VirtualBox VMs' \
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

    pushd ~
    echo sudo rsync -av --delete "${excludesflags[@]}" ./ $1
    sleep 2
    sudo rsync -av --delete "${excludesflags[@]}" ./ $1
    popd
}

fullbackup() {
    dobackup "$1" full
}

minibackup() {
    dobackup "$1" mini
}

# Something is writing to recently-used.xbel and I'm not sure what.
# This might help to monitor it.
alias recent='ls -l ~/recently-used.xbel*(.N) ~/.local/share/recently-used.xbel*'

# Torikun says this might work for talking to the raspberry pi.
# It has something to do with openvpn and might require running a DHCP
# server on the local machine.
alias piroute='iptables -t nat -A POSTROUTING -s 10.8.0.0/24 -o eth0 -j MASQUERADE'

alias znetspam="; echo -n '    Caught by znet: ' ; grep -h Subject ~/Spam/* | fgrep '[SPAM]' | wc -l; echo -n 'Not caught by znet: ' ; grep -h Subject ~/Spam/* | fgrep -v '[SPAM]' | wc -l"

alias pygrep="langgrep python"

alias tcolors='printf "\e[%dm%d dark\e[0m  \e[%d;1m%d bold\e[0m\n" {30..37}{,,,}'

alias chase='fincompare.py ~/bin/chase.py SRCMX FMAGX FPURX BFLAX LDLAX FUSVX VSCGX IRCAX SCAL'

alias moontrade='sshfs moon:/back/trade ~/moontrade'
alias unmoontrade='fusermount -u ~/moontrade'
alias remoontrade='sshfs -o reconnect moon:/back/trade ~/moontrade'

# rsync a local web directory to shallowsky.
toshallow() {
    localdir=$1
    # Make sure directories have a terminal slash,
    # whether or not the user provided one.
    if [ -d $localdir ]; then
        # Remove terminal slash.
        # ## requires extendedglob, so make sure it's set locally.
        setopt localoptions extendedglob
        localdir=${localdir%%/##}/
    fi
    plaindir=${localdir#~/}
    cmd="rsync -av $localdir shallowsky.com:$plaindir"
    # We'll went --delete here too, but let's hold off until it's known working.
    echo $cmd
    eval $cmd
}

# I can never remember the ever-changing CUPS commands to talk to
# printers from the cmdline.
alias printers='lpstat -s; echo; echo You can print with lp -d printername'

# Quick-jump to deeply nested directories
# http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
# Now if I could just remember to use it.
export MARKPATH=$HOME/.marks
function jump { 
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
alias ju=jump
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

# Mirror a specific ftp website:
ftpmirror() {
list_commands=""
for dir in thisdir thatdir theotherdir
do
  list_commands="$list_commands
mirror --only-newer -vvv -X '*.ppt' -X '*.doc*' -X '*.pdf' htdocs/$dir $HOME/web/ftpmirror/$dir"
done

echo Commands to be run:
echo $list_commands
echo

lftp <<EOF
open -u 'user,passwd' ftp.example.com
$list_commands
bye
EOF

# lftp doesn't preserve permissions, even though -p is supposed to.
# So change permissions back on the files that need it:
chmod 666 $HOME/web/peec/data/*_edited.csv
chmod 755 $HOME/web/peec/guides/save_db.cgi
}

# Copy a list of selected files up to the PEEC website using lftp.
# Assumes we already have a bookmark with the password which puts
# us already in htdocs.
topeec() {
    list_commands=""
    for f in $*; do
        dn=$(dirname $f)
        # bn=$(basename $f)
        list_commands="$list_commands
put -O $dn $f"
    done

    echo Commands to be run:
    echo $list_commands
    echo

    lftp peec <<EOF
$list_commands
bye
EOF
}

# Mounting PEEC website over ftp with curlftpfs.
# umount doesn't work, have to use fusermount -u.
alias peec="mount /peecweb"
alias unpeec="fusermount -u /peecweb"

# Android adb logcat is supposed to accept a filter argument to show only
# logs from a single program, but it doesn't work and I have to use grep.
# But just grepping for the program name gets tons of extra lines from
# every tap or other event delivered to the program;
# and piping grep to grep makes grep buffer its output
# unless the --line-buffered flag is specified.
# --line-buffered probably isn't needed on the last grep.
alias adbfeed='adb logcat | grep --line-buffered FeedViewer | grep -v --line-buffered Delivering'

phof () {
    pho `fotogr $*`
}

cleanspam() {
    for folder in $HOME/Spam/*; do
        if [[ -f $folder ]]; then
            echo $folder
            cat $folder >> $HOME/Spam/trained/$(basename $folder)
            cp /dev/null $folder
        fi
    done
    tail -7000 $HOME/Procmail/log >$HOME/Procmail/olog
    rm $HOME/Procmail/log
}

# I get tired of all the multiple steps to update gimp now that it
# requires three separate repositories.
# Make sure this exits on errors!
gimpmaster() {
    setopt localoptions errreturn

    pushd ~/outsrc/babl
    git pull
    make -j4
    sudo make install

    cd ~/outsrc/gegl
    git pull
    make -j4
    sudo make install

    cd ~/outsrc/gimp
    git pull
    make -j4
    sudo make install

    popd
}

# Two good pages on zsh scripting:
# http://www.rayninfo.co.uk/tips/zshtips.html
# http://www.linux-mag.com/id/1079/
