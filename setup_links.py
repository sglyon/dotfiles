from os import symlink, getcwd
from os.path import expanduser
from subprocess import call

this_dir = getcwd()
home = expanduser('~')

# Define directories
st2_dir = home + '/Library/Application Support/Sublime Text 2/Packages'
st3_pack_dir = home + '/Library/Application Support/Sublime Text 3/Packages'
st3_ipack_dir = home + '/Library/Application Support/Sublime Text 3/Installed Packages'
vimrc_dir = home + '/.vimrc'
vim_dir = home + '/.vim'
emacs_dir = home + '/.emacs.d'

# Create symlink for sublime Text 2
try:
    call(['rm', '-rf', st2_dir])
    symlink(this_dir + '/sublime/Packages/', st2_dir)
except OSError:
    # Maybe We don't have sublime text 2 installed?
    print("Couldn't create symlink for Sublime Text 2. Just skipping it")

# Create symlink for sublime Text 3
try:
    call(['rm', '-rf', st3_pack_dir])
    call(['rm', '-rf', st3_ipack_dir])
    symlink(this_dir + '/sublime3/Packages/', st3_pack_dir)
    symlink(this_dir + '/sublime3/Installed Packages/', st3_ipack_dir)
except OSError:
    # Maybe We don't have sublime text 3 installed?
    print("Couldn't create symlinks for Sublime Text 3. Just skipping it")

# Create symlinks for vim
try:
    call(['rm', '-rf', vim_dir])
    call(['rm', vimrc_dir])
    symlink(this_dir + '/vim/vimrc', vimrc_dir)
    symlink(this_dir + '/vim/', vim_dir)
except OSError:
    print("Couldn't create symlink for vim. Just skipping it")

# Create symlink for emacs
try:
    call(['rm', '-rf', emacs_dir])
    symlink(this_dir + '/emacs/', emacs_dir)
except OSError:
    print("Couldn't create symlink for emacs. Just skipping it")
