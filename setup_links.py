from os import symlink, getcwd
from os.path import expanduser
from subprocess import call


def attempt_symlink(source, target, clean=True):
    msg_f = "Couldn't create symlink for {0}. Just skipping it"
    msg_t = "Successfully created symlink for {0}"
    if clean == True:
        call(['rm', '-rf', target])
    try:
        symlink(source, target)
        print(msg_t.format(target) + '\n')
    except OSError:
        # Maybe We don't have sublime text 2 installed?
        print(msg_f.format(target))

# Define directories
this_dir = getcwd()
home = expanduser('~')

#st2_dir = home + '/Library/Application Support/Sublime Text 2/Packages'
st3_pack_dir = home + '/Library/Application Support/Sublime Text 3/Packages'
st3_ipack_dir = home + '/Library/Application Support/Sublime Text 3/Installed Packages'
vimrc_dir = home + '/.vimrc'
vim_dir = home + '/.vim'
emacs_dir = home + '/.emacs.d'

# Create symlink for sublime Text 2
# attempt_symlink(this_dir + '/sublime/Packages/', st2_dir)

# Create symlink for sublime Text 3
attempt_symlink(this_dir + '/sublime3/Packages/', st3_pack_dir)
attempt_symlink(this_dir + '/sublime3/Installed Packages/', st3_ipack_dir)

# Create symlinks for vim
attempt_symlink(this_dir + '/vim/vimrc', vimrc_dir)
attempt_symlink(this_dir + '/vim/', vim_dir)

# Create symlink for emacs
attempt_symlink(this_dir + '/emacs/', emacs_dir)

# Crete symlink for zshrc
attempt_symlink(this_dir + '/zsh/zshrc', home + '/.zshrc')
