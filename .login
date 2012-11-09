# Akkana's .login file

#set verbose
if ($?debug) echo -n "[.login] "

#
# Reset PRIMES, and take any primes out of the prompt
# which was already set in .cshrc:
#
setenv PRIMES ""
set noglob
set prompt = `echo $prompt | sed s/\'//g`
unset noglob
# sed cmd strips out the final space, so put it back in:
set prompt = "$prompt "

#
# set up the terminal
#

stty erase 

# Many Suns (like netcom) don't have resize.
# So prepare to do something else if necessary.
which resize >& /dev/null
if ($status) then
    alias res 'echo "Assuming 57 rows ..."; stty rows 57'
else if ($term == "sun") then
    alias res 'set noglob; eval `resize -s \!*`'
else
    alias res 'set noglob; eval `resize \!*`'
endif

if ( -r $HOME/tcap) setenv TERMCAP $HOME/tcap

# if ($term == 'iris-ansi-net') set term='iris-ansi'
#if (! $?WINDOWID && $term != 'xterm' && ! ($term =~ iris*) && ! ($term =~ sun*)) then
#  set noglob
#  eval `tset -s -Q -m ':?vt100' `
#else if ((`uname -s` != IRIX) && ($term =~ iris*) ) then
#  set term=vt100
#  setenv TERM vt100
#endif
#set term = $TERM
###stty echoe erase "^H" kill "^U" werase  intr "^C" eof "^D" susp "^Z"
# stty tabs hupcl ixon ixoff
if ($SHELL == "/bin/tcsh") then
    bindkey  backward-delete-word
endif

# If we're logged in on the console, do some housekeeping, then start X:
# This isn't working any more on Ubuntu, though:
# I guess we don't log in on tty1 any more?
echo Logged in on $tty : `date`
if ($tty == tty1) then
    # Remove flash cookies
    echo "Removing flash cookies"
    rm -rf $HOME/.macromedia

    #echo Starting X
    #startx >& $HOME/.xsession-errors

# Set the default X server in case we're here via rlogin:
else if ($?REMOTEHOST) then
    # Can't use && -- if REMOTEHOST isn't set the second clause gives an error.
    if ($REMOTEHOST != '') then
        setenv DISPLAY ${REMOTEHOST}:0
    endif
endif

if ($?debug) echo "."

echo "Emacs jump to bookmark: C-x r b"
echo "Emacs set bookmark: C-x r m"
cat /etc/motd
fortune | cowsay
