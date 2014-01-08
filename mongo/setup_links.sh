# Get path to this file -- store it as DIR
DIR="$( cd "$( dirname "$0" )" && pwd )"

# set up link for .monogdb.js file
ln -s $DIR/mongorc.js $HOME/.mongorc.js
