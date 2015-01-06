########################
# Akkana's .cshrc
#########################

#set debug=1
#set verbose
if ($?debug) echo -n "[.cshrc] "

if ( ! $?prompt || ! $?term ) then
  # not interactive -- skip the hard stuff
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
    # Keep the linux stuff here, since it's most likely to be used
    set path = ( $HOME/bin/linux $HOME/bin /usr/local/bin /usr/sbin /usr/bin /bin /usr/bin/X11 . /sbin /usr/games $HOME/android/eclipse $HOME/android/android-sdk-linux_x86/tools /opt/meego/meego-sdk-qt-creator/bin )

    setenv PAGER less
    # On Gentoo, manuals only work if LESS is set to -R -M --shift 5
    if (-e /etc/gentoo-release) then
      setenv LESS '-E -R -M --shift 5'
    else
      setenv LESS -E
    endif
    setenv EDITOR vim
#    if ( $TERM == 'xterm' && -f /usr/share/terminfo/x/xterm-256color ) then
#      setenv TERM xterm-256color
    endif

    # Make patch actually obey -p0
    # setenv POSIXLY_CORRECT 1

    # ls 4.0.36 changed its sort so that by default, dot and non-dot files
    # are grouped together.  Apparently this is a POSIX misfeature and
    # can be turned off thusly:
    setenv LC_COLLATE C

    # For mutt:
    #setenv LC_ALL en_US.UTF-8

    # Linux doesn't have enscript, but it has mprint:
    alias ens 'mpage -2 -f -p \!* | lpr'
    alias ens4 'mpage -4 -f -p \!* | lpr'

    # CUPS on Debian with the C86 prints normal text starting
    # two characters off the left side of the page.
    # lp -o page-left=16 will print it pretty much flush.
    # But by default I want some margins, so:
    #alias lpr "lp -o page-left=38 -o page-top=17"
    # Turns out that xpp (X printer panel) can adjust margins systemwide!
    # Except it may not actually work.

    # But on Karmic, it has another bug and I have to set the cpi
    # explicitly wrong:
    #alias lpr /usr/bin/lpr -o cpi=17

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

set history=200
set filec

alias       m       /usr/bin/mutt

alias       beep    echo 
alias       pop     play -q $HOME/.xchat2/sounds/pop.wav
alias       pd      pushd
alias       pd2     pushd +2
alias       ls      ls -FH
alias       ll      ls -lsFhH
alias       lla     ls -AlsFhH
alias       llt     ls -lsFhtH
alias       llth    '/bin/ls -lshHFt \!* | head -20'
alias       j       jobs
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
# Java
alias	av	appletviewer
alias	jc	javac

alias	df	df -k

# halt and reboot don't always work on the Vaio,
# and can't be run suid.
alias   off     sudo shutdown -h now
alias   halt    sudo shutdown -h now
alias   reboot  sudo shutdown -r now

# Check what sort of line endings a file has -- courtesy of pjf:
#alias lineendings 'perl -lne\'print /\r$/ && "CR","LF"; exit\''
# and another clever way from Kathryn:
#alias lineendings2 'head -n 1 \!*  | sed -e \'s/<.*>//\' | file -'
# <kathryn> i'd rather just do sed -e 's/^.*//' but that replaces
#           whatever line endings you have with \n

# Get ink status from Epson printers
alias   epsonink     escputil -r /dev/usblp0 -mC86 -u -i
alias   epsonnozzle  escputil -r /dev/usblp0 -mC86 -u -n
alias   epsonclean   escputil -r /dev/usblp0 -mC86 -u -c

# Pluck/scoop news sites for the Palm:
#setenv  PILOTRATE 57600
# PILOTPORT needs to be /dev/ttyUSB0 for Tungsten, /dev/ttyUSB0 all else.
setenv PILOTPORT /dev/ttyUSB0
alias   pluck 'plucker-build -f plucked -H \!* --noimages --stayonhost --zlib-compression'

# Sitescooper doesn't let these be configured in sitescooper.cf:
# noheaders/nofooters screws up I,Cringely
#alias   scoop     'setenv LANG C; rm $HOME/.sitescooper/prc/*.pdb; \
#                   sitescooper -debug \!* |& setsid tee $HOME/scoop.out; \
#                   cat $HOME/web/xtraurls.html >> $HOME/.../xtraurls.html; \
#                   rm $HOME/web/xtraurls.html; \
#                   ls $HOME/.sitescooper/prc'
#alias   rscoop "rm $HOME/.sitescooper/prc/*; nohup sitescooper >& $HOME/scoop.out &"
#alias   scoopsite "sitescooper -sites \!*"
#unset   scoopcmd
#alias   piscoop  'pilot-xfer -i ~/.sitescooper/prc/*.pdb'
#alias   pifeed  'pilot-xfer -i ~/.plucker/feedme/*.pdb'
#alias   pils     'echo "In ~/.plucker/feedme : "; ls ~/.plucker/feedme'
#alias   picd     'cd ~/.sitescooper/prc'
#alias   palmbak  'set palmname = \!:1; \
#                  rm -rf ~/Pilot/$palmname.bak; \
#                  cp -a ~/Pilot/$palmname ~/Pilot/$palmname.bak; \
#                  pilot-xfer -s ~/Pilot/$palmname; \
#                  pluckmemos -f $HOME/Pilot/$palmname/MemoDB.pdb; \
#                  if (-f ~/.plucker/html/xtraurls.html) cat ~/.plucker/html/xtraurls.html >>$HOME/Docs/Lists/xtraurls.html '

#alias zirebak  'setenv PILOTPORT /dev/ttyUSB1; palmbak Zire'
#alias tungbak  'setenv PILOTPORT /dev/ttyUSB0; palmbak Tungsten'
#alias cliebak 'setenv PILOTPORT /dev/ttyUSB1; palmbak Clie'
#alias treobak 'setenv PILOTPORT /dev/ttyUSB1; palmbak Treo'

alias feed 'rsync -av --delete moon:.cache/feedme/ ~/.cache/feedme/ \
            mount /droidsd; \
            urlrss; \
            feedme; \
            echo Copying `date +%m-%d-%a` to /droidsd/feeds/ ; \
            cp -r ~/feeds/`date +%m-%d-%a` /droidsd/feeds/ ; \
            umount /droidsd &; \
            echo "Syncing back to the server"; \
            rsync -av ~/.cache/feedme/ moon:.cache/feedme/'

# Clavius has moby-thesaurus locally, but default dict goes to moon.
alias   thes      dict -h localhost -d moby-thesaurus
# Translation dictionaries:
alias en-spa      dict -h localhost -d fd-eng-spa
alias spa-en      dict -h localhost -d fd-spa-eng
alias en-lat      dict -h localhost -d fd-eng-spa
alias lat-en      dict -h localhost -d fd-lat-eng

# Turn flash on and off in Firefox, because as of FF3 it crashes
# so often, even when blocked by flashblock:
#alias flashoff 'mv $HOME/.mozilla/plugins/libflashplayer.so $HOME/.mozilla/plugins/flashplayer.xpt $HOME/.mozilla/noplugins'
#alias flashon 'mv $HOME/.mozilla/noplugins/libflashplayer.so $HOME/.mozilla/noplugins/flashplayer.xpt $HOME/.mozilla/plugins'

###############################
# Trn from multiple servers
###############################
# trn options: add -e -L for screen clearing (but it doesn't work right),
# omit -x and -X options for regular non-threaded rn
# -q is supposed to ask about new groups, but doesn't.
# But doing "a*" gets a prompt for all the newsgroups.
#setenv TRNINIT "-C7 -I -M -s -x -X15X -m=u -EATTRIBUTION='%)f <%>f> wrote:' -ESAVEDIR=$HOME/Msgs"
# Can add "-EXTERMMOUSE=y" to TRNINIT but then you can't
# select with the mouse any more
# $HOME/.newsrc is for news.mozilla.org.
# To use other .newsrcs, change DOTDIR and NNTPSERVER
#alias moztrn "setenv NNTPSERVER news.mozilla.org; setenv DOTDIR $HOME/nntpservers/news.mozilla.org; \trn"

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
if ($?tcsh) then
    #set standout = "%U"
    #set standout_end = "%u"
    set standout = "%B"
    set standout_end = "%b"
else if ( $term =~ iris* ) then
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
else if ($term == "xterm") then
    set standout = "[7m"
    set standout_end = "[m"
else if ($term == "vt100") then
    # a couple of items for pcplus and netcom
    if ( -x /usr/5bin/tabs ) /usr/5bin/tabs
    set standout = "[7m"
    set standout_end = "[m"
else if ($term == "ansi" || $term == 'vt102') then
    stty -tabs
    set standout = "[7m"
    set standout_end = "[m"
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

###########################################
# Colorizing for other apps, e.g. man/less
###########################################
# Colors for man pages. See
# http://wiki.clug.org.za/wiki/Colour_on_the_command_line
#
# Black       0;30     Dark Gray     1;30
# Blue        0;34     Light Blue    1;34
# Green       0;32     Light Green   1;32
# Cyan        0;36     Light Cyan    1;36
# Red         0;31     Light Red     1;31
# Purple      0;35     Light Purple  1;35
# Brown       0;33     Yellow        1;33
# Light Gray  0;37     White         1;37
#
setenv LESS_TERMCAP_mb '[01;31m'
setenv LESS_TERMCAP_md '[01;31m'
setenv LESS_TERMCAP_me '[0m'
setenv LESS_TERMCAP_se '[0m'
setenv LESS_TERMCAP_so '[01;44;33m'
setenv LESS_TERMCAP_ue '[0m'
setenv LESS_TERMCAP_us '[01;35m'

###############################
# Location-specific stuff:
###############################

#
# from here to near the end, Company-specific stuff:
#
set company=netscape

switch ($company)

# Settings for Netscape:
  case 'netscape':

    #
    # Mozilla build
    #
    setenv CVSROOT :ext:akkzilla%shallowsky.com@cvs.mozilla.org:/cvsroot
    alias cvsblame      cvs annotate

    alias build "gmake -f client.mk build; echo "
    alias pullfox "gmake -f client.mk checkout MOZ_CO_PROJECT=browser"
    alias newmoz "cvs co mozilla/client.mk; cd mozilla; gmake -f client.mk pull_all build_all; echo "

    alias t 'set _moz=`trees \!*`; if ("$_moz" != "") cd "$_moz"; endif; unset _moz; pwd'
    alias mozconfig "../mozilla/configure --enable-editor --enable-debug --with-pthreads --enable-md; echo "
    alias ml \
      'foreach mdir (../build* ../../build* ../../../build* ) \
        echo updating $mdir \
        pushd $mdir \
        rm -f lib* \
        gmake \
	popd \
      end'
#    setenv LD_LIBRARY_PATH .

    # Helper for tkdiff
    alias cdiff 'foreach fil ( \!* ) \
      tkdiff $fil & \
    end'

    # Testing moz in various forms:
    alias moz ./mozilla -P Debug
    alias mozed ./mozilla -P Debug -edit

    # Here's how to dump core on assertions:
    # stack=dump core, trap=break in gdb
    #setenv XPCOM_DEBUG_BREAK stack

    alias update        "cvs update \!* |& egrep -v '^cvs' ; echo "
    alias updaten      "cvs -n update \!* |& egrep -v '^cvs' ; echo "
    alias showmod      "cvs -n update \!* |& egrep -v '^cvs' |& egrep -v '^U' ; echo "
    alias log          "cvs log \!* | grep -v '^	' | more"

    alias ssera sudo /etc/init.d/vpn start
    alias sera open_tunnel -e -n 443 sera25-mv.aoltw.net akkana
    alias sera2 open_tunnel -e -n 443 sera25-rtc.aol.com akkana
    alias nosera close_tunnel

    breaksw

# end of company-specific stuff
  endsw

endif
# end of "only if interactive (i.e. prompt set)"

# echo "."

if ($?debug) then
  echo "."
  unset debug
endif

setenv PHO_ARGS p

alias newraptor "cvs co mozilla/client.mk; cd mozilla; gmake -f client.mk"
umask 22

alias sargepkg  'links "http://packages.debian.org/cgi-bin/search_packages.pl?keywords=\!*&searchon=names&subword=1&version=testing&release=all"'
alias sidpkg    'links "http://packages.debian.org/cgi-bin/search_packages.pl?keywords=\!*&searchon=names&subword=1&version=unstable&release=all"'
alias sargefile 'links "http://packages.debian.org/cgi-bin/search_contents.pl?word=\!*&searchmode=searchword&case=insensitive&version=testing&arch=i386"'
alias sidfile   'links "http://packages.debian.org/cgi-bin/search_contents.pl?word=\!*&searchmode=searchword&case=insensitive&version=unstable&arch=i386"'
limit coredumpsize unlimited

# gnumeric refuses to remember window size.  So try to force it.
alias gnumeric gnumeric -g 800x600

# mencoder options are black magic.  
# This works for converting Minolta quicktime .mov to mpeg:
alias mov2mpg1 'mencoder \!:1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg1video -o \!:2'
# -lavc is ffmpeg, and the default codec is divx:
alias mov2divx 'mencoder \!:1 -oac pcm -ovc lavc -o \!:2'
# From drc on #gimp:
alias mov2mpeg4 'mencoder \!:1 -oac pcm -ovc lavc -lavcopts vcodec=mpeg4:vqmin=2:vlelim=-4:vcelim=9:lumi_mask=0.05:dark_mask=0.01:vhq -o \!:2'

# use ffmpeg to convert to flash: the -ar 44100 isn't always needed,
# but for input videos that sample at 48000, ffmpeg will die with an error.
alias mov2flv 'ffmpeg -i \!:1 -ar 44100 \!:2'

# Extract the audio from a flash (e.g. youtube) track:
# Thanks http://en.gibney.org/convert_flv_files_to_mp3/ and
# http://en.gibney.org/youtube_to_mp3/
alias flv2mp3 'ffmpeg -i \!:1 -f mp3 \!:2'

# This may not work with 2.6.  Note, cdrecord dev=ATA: -scanbus
# as per /usr/share/doc/cdrecord/README.ATAPI.setup
alias burniso 'cdrecord -v speed=0 \!*'
#alias cddup "cdrecord -v speed=0 -isosize /dev/cdrom"

# Recipes for ebook-convert
alias pdf2epub 'ebook-convert \!:1 `echo \!:1 | sed "s/.pdf/.epub/"` --authors "\!:2" --title "\!:3"'

# alias pho pho -d

# Remove the line matching $1 from ~/.ssh/known_hosts.
# Ssh refuses to operate if anything has changed about the host:
# network card, distro it's running, etc.
alias sshrm 'mv $HOME/.ssh/known_hosts $HOME/.ssh/known_hosts.bak; grep -v \!^ $HOME/.ssh/known_hosts.bak >$HOME/.ssh/known_hosts'

# Get the temperature from /proc/acpi/thermal_zone/THRM/temperature
# and convert it to F
alias xxtemp "cat /proc/acpi/thermal_zone/THRM/temperature; \
            awk '{print $2}' < /proc/acpi/thermal_zone/THRM/temperature | xargs -i units 'tempC({})' tempF"

# Screenshot for Apress style guidelines: tiff, lzw compression.
alias bookss "import -frame -density 150x150 -label "Figure 2-1: The Toolbox" -compress LZW"

# Record a realaudio stream.
# This used to work to mp3 but now it only works to wav.
alias getreal 'mplayer -playlist \!:1 -ao pcm:file=\!:2 -vc null -vo null'
# Then transcode it with: 
# lame --tg Other --ta artist -tl album file.wav file.mp3

alias ratings 'links -dump "http://www.amazon.com/gp/product/1590595874" | grep "in Books" | grep -v Explore'
alias ratings2 'links -dump "http://www.amazon.com/gp/product/1430210702" | grep "in Books" | grep -v Explore'

#alias newbg 'xsetbg -fullscreen -border black `find $HOME/Backgrounds/ -name "*.*" | randomline`'
alias newbg 'hsetroot -center `find -L $HOME/Backgrounds/ -name "*.*" | randomline`'

alias white xsetroot -bg white

alias sp 'spell \!* | sort | uniq'

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

alias falconcam2 vlc http://falconcam.sanjoseca.gov/nest
alias falconcam vlc rtsp://bird-mirror.ucsc.edu/birdie-sj.sdp

# How to run xnest, in case they ever fix it
alias xxnest 'xinit ~/.xinitrc.xnest -- /usr/bin/Xnest -ac :1 -geometry 1024x768'
# Start a second X server on F8 -- this only works from tty2
alias secondx 'startx $HOME/.xinitrc.xnest -- :1'

# Image to help with resistor color codes
alias resist pho -P $HOME/Docs/hardware/resistors.jpg

# I can never remember the name audacious, and besides, it doesn't
# autocomplete because of audacity.
alias xmms audacious

# For the Apple cinema display:
#alias brighter /home/akkana/bin/linux/acdcontrol /dev/usb/hiddev0 +10
#alias dimmer /home/akkana/bin/linux/acdcontrol /dev/usb/hiddev0 -- -10
#alias screenoff 'sleep 1; xset s activate'

alias xb '@ b1 = \!:1; @ b2 = $b1 * 65536 / 100 - 1; xbrightness $b2; unset b1 b2'

# Print only odd or even pages on a printer (for dual-sided printing):
alias lpodd lp -o page-set=odd
alias lpeven lp -o page-set=even

# Try to remember ssh password, for version control systems:
# http://automorphic.blogspot.com/2007/01/using-ssh-agent-to-stop-getting.html
alias sshpwd 'eval `/usr/bin/ssh-agent`; ssh-add ~/.ssh/id_rsa'

# I can never remember this name, and gtkfontsel has gone away, so:
alias gtkfontsel "echo Use gnome-specimen instead"

# Some useful ps arguments:
alias pscpu ps axSO -C o user,pid,pcpu,pmem,vsz,rss,stat,time,comm
alias psmem ps axSO -r o user,pid,pcpu,pmem,vsz,rss,stat,time,comm

# Find all files beneath the current dir that lack a Keywords file:
# (switched to using a more reliable python script).
# alias nokeywords "find . -type d -exec ls '{}/Keywords' \; |& grep -v \\./Keywords | grep 'No such file' | sed -e 's_.*cannot access \./__' -e 's_/Keywords: No such.*__' "

# Remove podget podcast files except recent ones:
#alias prunepod 'find ~/Archive/Sounds/talk  -maxdepth 1 -name "*.mp3" -ctime +6 -exec rm "{}" \;'
alias prunepod 'find ~/POD -ctime +20 -exec rm "{}" \;'

# Show podcasts recently downloaded:
alias pods 'cd ~/POD; ls -1t `find . -type f ` | head -30'

# cryptmount device name
alias cryptmount 'sudo cryptsetup luksOpen \!:1 \!:2; sudo mount /dev/mapper/\!:2 /media/\!:2'
alias world cryptmount /dev/sdb2 world
alias unworld sudo umount /media/world

# Gentoo aliases
alias update "emerge --update --deep --newuse world"

# updating gentoo:
# emerge --sync
# emerge -puD world
#   OR
# emerge-delta-webrsync -u
#   THEN:
# emerge -uD world
# eix-update

# Python snippets
setenv SNIPPETS_DIR /home/akkana/outsrc/python-snippets

# For ubuntu development
setenv DEBFULLNAME "Akkana Peck"
setenv DEBEMAIL "akkana@shallowsky.com"

# Serial connections to plugs and other embedded devices:
alias serial screen /dev/ttyUSB0 115200
alias serial1 screen /dev/ttyUSB1 115200

# ssh to get around stupid non-turnoffable known hosts checking
# http://linuxcommando.blogspot.com/2008/10/how-to-disable-ssh-host-key-checking.html
#alias sssh  ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no 

# Meego OSC
alias mosc 'osc -Ahttps://api.pub.meego.com --no-keyring --no-gnome-keyring \!*'

# Update website blog entries
alias blogup 'cd ~/web/blogfiles; pyblosxom-cmd staticrender --incremental; cd'
alias blogupdate 'cd ~/web/blogfiles; pyblosxom-cmd staticrender --incremental && ~/bin/blogtopics && mv ../blog/topics.html ../blog/oldtopics.html && mv ../blog/newtopics.html ../blog/topics.html && blog-tag-index; cd'

