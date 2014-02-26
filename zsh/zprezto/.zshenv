#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

path=(
  /usr/local/anaconda/bin
  /usr/local/anaconda/python.app/Contents/MacOS
  /usr/texbin
  $HOME/bin
  /usr/local/{bin,sbin}
  /usr/local/anaconda/envs/python3/bin
  $path
)

