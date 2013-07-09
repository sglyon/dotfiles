from subprocess import call as _call
import sys
import os
from platform import system


def call(call_str):
    """
    Use subprocess.call with a string with spaces, not list of strings
    """
    _call(call_str.split(' '))

# Get the operating system
my_system = system()
if my_system.lower().startswith('darw'):
    my_os = 'OSX'
elif my_system.lower().startswith('lin'):
    my_os = 'Linux'
else:
    print("Windows?!\nReally?\n...\nExiting now.")
    sys.exit()


if my_os == 'OSX':
    # Check for OSX command line tools:
    try:
        call('gcc')
    except OSError:
        print("You haven't installed the OSX command line tools yet. " +
              "Do that now")
        print('I will even open the browser for you')
        import webbrowser as web
        web.open('https://developer.apple.com/downloads/index.action#')
        sys.exit()

    # Check for homebrew
    try:
        call("brew --version")
    except OSError:
        print("You didn't have homebrew installed. I will do that now")
        hb = 'ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"'
        call(hb)

        # Install brew-cask
        call("brew tap phinze/homebrew-cask")
        call("brew install brew-cask")

    # Install certain casks
    want_to_install = "Would you like to install "
    popular_cask = ['google-chrome', 'alfred', 'emacs', 'cyberduck', 'dropbox',
                    'f-lux', 'google-drive', 'iterm2', 'kaleidoscope',
                    'libre-office', 'macvim', 'pycharm', 'rstudio', 'skype',
                    'spotify', 'sublime-text', 'transmit']
    to_install = {}
    for i in popular_cask:
        x = str(raw_input("Would you like to install %s (y/n)?: " % i))
        if x.startswith('y'):
            to_install[i] = True
        else:
            to_install[i] = False

    installing = []
    for key in to_install.iterkeys():
        if to_install[key]:
            installing.append(key)

    print('You have chosen to install %i casks. ' % (len(installing)) +
          'We will do so now. It might take a while so kick back and relax.')

    bci = "brew cask install "
    for i in installing:
        print('About to install %s' % (i))
        call(bci + i)
        print('Finished installing %s' % (i))

    call("brew cask linkapps")
    # cal(bci + "google-chrome")

    # Install other homebrew forumlas
    brew_list = ['doxygen', 'eigen', 'graphviz', 'swig']
    to_install = {}
    installing = []
    for i in brew_list:
        x = str(raw_input("Would you like to install %s (y/n)?: " % i))
        if x.startswith('y'):
            to_install[i] = True
            installing.append(i)
        else:
            to_install[i] = False

    print('You have chosen to install %i formulas. ' % (len(installing)) +
          'We will do so now. It might take a while so kick back and relax.')

    for i in installing:
        print('About to install %s' % (i))
        call("brew install " + i)
        print('Finished installing %s' % (i))
