# .zprofile happens *before* .zshrc.
# Use .zlogin for anything that should happen after it.

export primes=''

# This had something to do with rust, maybe for the mozilla build,
# but I'm not sure and it interferes with python venvs.
# export PATH="$HOME/.cargo/bin:$PATH"
