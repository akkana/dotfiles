########################
# Akkana's .cshrc
#########################

# set debug=1
# set verbose
if ($?debug) echo -n "[.cshrc] "

if ( ! $?prompt || ! $?term ) then
  # not interactive -- skip most of this file.
  unset debug
  exit 0
endif

# Do case insensitive completion
#set complete=enhance

# OS-specific stuff
set opsys = `uname -s`

switch ($opsys)
  case IRIX:
    source $HOME/.cshrc.irix
    breaksw

  case Linux:
    if ($?debug) then
      echo Linux
    endif
    # Keep the linux stuff here, since it's most likely to be used
    set path = ( $HOME/bin/linux $HOME/bin /usr/local/bin /usr/sbin /usr/bin /bin /usr/bin/X11 . /sbin /usr/games $HOME/android/eclipse $HOME/android/android-sdk-linux_x86/tools /opt/meego/meego-sdk-qt-creator/bin )
    # $HOME/CodeSourcery/Sourcery_G++_Lite/bin
    if ($?debug) then
      echo set path
    endif

    # Some hosts, like Brownrice and SWXP, don't have rxvt-unicode-256color
    # tcsh: No entry for terminal type "rxvt-unicode-256color"
    # tcsh: using dumb terminal settings.
    # I'm not sure where those come from or how to stop them.
    # Weirdly, they happen when executing setenv LC_COLLATE C,
    # a few lines below.
    if ($TERM == rxvt-unicode-256color && \
        ! -e /usr/lib/terminfo/r/rxvt-unicode-256color && \
        ! -e /usr/share/terminfo/r/rxvt-unicode-256color) then
      setenv TERM rxvt-256color
      echo ".login: resetting TERM to $TERM"
    endif
    if ($?debug) then
      echo fixed terminal
    endif

    setenv PAGER less
    # On Gentoo, manuals only work if LESS is set to -R -M --shift 5
    if (-e /etc/gentoo-release) then
      setenv LESS '-E -R -M --shift 5'
    else
      setenv LESS -E
    endif

    setenv EDITOR vim

    # Make patch actually obey -p0
    # setenv POSIXLY_CORRECT 1

    # ls 4.0.36 changed its sort so that by default, dot and non-dot files
    # are grouped together.  Apparently this is a POSIX misfeature and
    # can be turned off thusly:
    setenv LC_COLLATE C

    # For mutt:
    #setenv LC_ALL en_US.UTF-8


    breaksw

  case SunOS:
    source $HOME/.cshrc.sunos
    breaksw

  default:
    set path = ( $HOME/bin /usr/sbin /usr/bin /usr/ucb /usr/local/bin /usr/java/bin /usr/bsd /bin /usr/bin/X11 . )
    breaksw
endsw
# end of switch over different OSes

###############################
# General aliases and settings
###############################

# Alias to set the title on xterm compatibles:
alias titlebar     "echo ']2;\!*'"
alias ntitlebar     "echo '_;\!*\\'"

# Set some editing bindings for tcsh:
if ($?tcsh) then
  bindkey ^W backward-delete-word
  set autolist=ambiguous
endif

set notify

set mail = (0 $HOME/Msgs/in/Inbox /var/mail/akkana)

unset autologout
# setenv MORE -cs
#setenv EXINIT "set ai sm wm=9"
#setenv ENSCRIPT -2r

# Make rsync work, since ssh is the only option actually enabled
# in most linux distros:
setenv RSYNC_RSH ssh

setenv PHO_ARGS p

set history=200
set filec

alias       m       /usr/bin/mutt

alias       beep    echo 
alias       akk     play -q $HOME/.xchat2/sounds/akk.wav
alias       pop     play -q $HOME/.xchat2/sounds/pop.wav
alias       pd      pushd
alias       pd2     pushd +2
alias       ls      ls -FH
alias       ll      ls -lsFhH
alias       lla     ls -AlsFhH
alias       llt     ls -lsFhtH
alias       llth    '/bin/ls -lshHFt \!* | head -20'
alias       j       jobs
alias       ph      'grep -i \!* ~/.../Phones'
alias       ap      man -k
alias       s       suspend
alias       xx      'startx >& .xsession-errors'
alias       mpl2    mplayer -zoom -xy 2
alias       mpl3    mplayer -zoom -xy 3

###############################
# Recursive greps
alias       gr      "find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' -and -not -name '*.pdf' -and -not -name '*.doc' \) -print0 | xargs -0 grep \!* /dev/null | fgrep -v .svn"
alias       ggr      "find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 grep \!* /dev/null | grep -v 'Binary file'"
alias       cgr     "find . \( -name '*.[CchH]' -or -name '*.cpp' -or -name '*.cc' \) -print0 | xargs -0 grep \!* /dev/null"
alias       hgr     "find . \( -name '*.h' -or -name '*.idl' \) -print0 | xargs -0 grep \!* /dev/null"
#alias       rgr     "find . \( -name '*.rb' -or -name '*.rhtml' \) -print0 | xargs -0 grep \!* /dev/null | fgrep -v .svn | fgrep -v /db/"
alias       rgr     "find . \( -name .svn -or -name db -prune \) -or \( -name '*.rb' -or -name '*.rhtml' -or -name '*.yml' \) -print0 | xargs -0 grep \!* /dev/null"
alias       jcgr    "find . \( -name '*.cChH' -or -name '*.cpp' -or -name '*.cc' -or -name '*.js' \) -print0 | xargs -0 grep \!* /dev/null"
alias       htgr    "find . -name '*.*htm*' -print0 | xargs -0 grep \!* /dev/null"
alias       jgr     "find . -name '*.js' -print0 | xargs -0 grep \!* /dev/null"
alias       xgr     "find . \( -name '*.cChH' -or -name '*.cpp' -or -name '*.xul' -or -name '*.html' -or -name '*.js' -or -name '*.css' \) -print0 | xargs -0 grep \!* /dev/null"
alias       cssgr   "find . -name '*.css' -print0 | xargs -0 grep \!* /dev/null"
alias       mgr     "find . -name '*akefile*' -print0 | xargs -0 grep \!* /dev/null"
alias       tgr     "find . -type f -and -name '*.txt' -print0 | xargs -0 grep \!* /dev/null | fgrep -v .svn"
alias       agr     "find . -type f -print0 | xargs -0 grep \!* /dev/null"
alias       zgr      "find . \( -type f -and -not -name '*.o' -and -not -name '*.so' -and -not -name '*.a' \) -print0 | xargs -0 zgrep \!* /dev/null"

###############################

# halt and reboot don't always work on the Vaio,
# and can't be run suid.
alias   off     sudo shutdown -h now
alias   halt    sudo shutdown -h now
alias   reboot  sudo shutdown -r now


###############################
# Terminal setting:
###############################

set standout = ""
set standout_end = ""
#if ( $?COLORTERM && $COLORTERM == gnome-terminal ) then
#    # gnome-terminal doesn't colorize bold, so we need a separate clause.
#    echo "Ewwww, gnome-terminal!"
#    setenv TERM xterm-noalt
#    # However, it also doesn't handle color escape sequences in prompts. Sigh.
#    set standout = "%B"
#    set standout_end = "%b"
# if ($?tcsh) then
#     #set standout = "%U"
#     #set standout_end = "%u"
#     set standout = "%B"
#     set standout_end = "%b"

if ( $term =~ iris* ) then
    # Set the prompt to be yellow.  This puts the prompt in boldface:
    # set standout = "[1m"
    # set standout_end = "[m"
    # but this is more reliable, setting the color explicitly:
    set standout = "[33m"
    set standout_end = "[37m"
    # colors: 30-37  Set the text color to black, red, green, yellow, blue,
    # magenta, cyan or white, respecotively (ISO 6429).
    # 40-47  Set the page color to black, red, green, yellow, blue,
    # magenta, cyan or white, respectively (ISO 6429).

else if ( $term =~ hp* ) then
    set standout = "&v2S"
    set standout_end = "&v3S"

else if ( $term =~ *256color ) then
    # Bold and purple
    set standout = "%{\033[1;38;5;56;56m%}"
    set standout_end = "%{\033[0m%}"

else
    # These used to work, but don't any more.
    # set standout = "[7m"
    # set standout_end = "[m"
    # Instead, use these:
    set standout = "%{\033[1;32m%}"
    set standout_end = "%{\033[0m%}"

endif

###############################
# Prompt setting:
###############################
# and set the prompt with the correct number of primes in it:
if (! $?PRIMES) setenv PRIMES ""
setenv PRIMES $PRIMES\'

# SunOS has different args for hostname than everyone else, sigh.
# It also doesn't have whoami.
if ($opsys == SunOS) then
  set host = `hostname`
  set me = "akkana"
else
  set host = `hostname -s`
  set me = `whoami`
endif

if ($me == 'akkana') then
  set prompt = "$standout($host$PRIMES)-$standout_end "
else if ($me == 'root') then
  # square brackets in next line cause a "variable syntax" error, so:
  set prompt = "$standout<$host#$PRIMES>-$standout_end "
else
  set prompt = "$standout($me@$host$PRIMES)-$standout_end "
endif

unset me
unset host
unset standout
unset standout_end

######################
# assorted aliases
######################

# gnumeric refuses to remember window size.  So force it:
alias gnumeric gnumeric -g 800x600

alias ratings 'links -dump "http://www.amazon.com/gp/product/1590595874" | grep "in Books" | grep -v Explore'
alias ratings2 'links -dump "http://www.amazon.com/gp/product/1430210702" | grep "in Books" | grep -v Explore'

# Suspend to RAM
if (-x /usr/sbin/pm-suspend) then
  alias zzz sudo /usr/sbin/pm-suspend --auto-quirks
  alias hib sudo /usr/sbin/pm-hibernate --auto-quirks
else if (-x /etc/acpi/suspend.sh) then
  alias zzz sudo /etc/acpi/suspend.sh
  alias hib sudo /etc/acpi/hibernate.sh
else if (-x /usr/sbin/hibernate) then
  # add --force if necessary
  alias zzz 'sudo hibernate -v 4 -F /etc/hibernate/ram.conf'
  alias hib hibernate
endif

# How to run xnest, in case they ever fix it
alias xxnest 'xinit ~/.xinitrc.xnest -- /usr/bin/Xnest -ac :1 -geometry 1024x768'
# Start a second X server on F8 -- this only works from tty2
alias secondx 'startx $HOME/.xinitrc.xnest -- :1'

# Image to help with resistor color codes
alias resist pho -P $HOME/Docs/hardware/resistors.jpg

alias lpodd lp -o page-set=odd
alias lpeven lp -o page-set=even

# cryptmount device name
alias cryptmount 'sudo cryptsetup luksOpen \!:1 \!:2; sudo mount /dev/mapper/\!:2 /media/\!:2'
alias world cryptmount /dev/sdb2 world
alias unworld sudo umount /media/world

# Python snippets
setenv SNIPPETS_DIR /home/akkana/outsrc/python-snippets

# For ubuntu development
setenv DEBFULLNAME "Akkana Peck"
setenv DEBEMAIL "akkana@shallowsky.com"

alias serial screen /dev/ttyUSB0 115200
alias serial1 screen /dev/ttyUSB1 115200

# Update website blog entries
alias blogup 'cd ~/web/blogfiles; pyblosxom-cmd staticrender --incremental; cd'
alias blogupdate 'cd ~/web/blogfiles; pyblosxom-cmd staticrender --incremental && ~/bin/blogtopics && mv ../blog/topics.html ../blog/oldtopics.html && mv ../blog/newtopics.html ../blog/topics.html && blog-tag-index; cd'

if ($?debug) then
  echo "."
  unset debug
endif
