    from subprocess import call as _call
import os
import sys


def call(call_str):
    """
    Use subprocess.call with a string with spaces, not list of strings
    """
    _call(call_str.split(' '))

home = os.path.expanduser('~')
try:
    os.makedirs(home + '/Python/Open_Source')
except OSError:
    pass
try:
    os.makedirs(home + '/Python/Sites')
except OSError:
    pass
try:
    os.makedirs(home + '/School/BYU')
except OSError:
    pass
try:
    os.makedirs(home + '/School/NYU')
except OSError:
    pass
try:
    os.makedirs(home + '/Research')
except OSError:
    pass

# Check for ssh keys
# print('I am going to check for ssh keys. If I find them, I am going to' +
#       '\nassume that you have put them up on github.')
# try:
#     os.chdir('~/.ssh')
# except OSError:
#     print("I couldn't move to ssh directory. You need to make a key")
#     print("Follow the instructions on the website I am opening for you")
#     import webbrowser as web
#     web.open('https://help.github.com/articles/generating-ssh-keys')
#     print('When you are done, run me again')
#     sys.exit()

# clone open-source projects
python_OpenSource = ['git@github.com:spencerlyon2/xdress.git',
                     'git@github.com:spencerlyon2/pandas.git',
                     'git@github.com:spencerlyon2/dolo.git']

for repo in python_OpenSource:
    folder_name = repo.split('/')[-1][:-4]
    repo_dir = home + '/Python/Open_Source/%s' % (folder_name)
    call('git clone %s "%s"' % (repo, repo_dir))

# Clone Personal things
call('git clone git@github.com:spencerlyon2/dotfiles.git' +
     ' "%s/dotfiles"' % (home))

call('git clone git@github.com:spencerlyon2/pytools.git' +
     ' "%s/Python/pytools"' % (home))

# Clone school things
call('git clone git@github.com:spencerlyon2/BYUclasses.git' +
     ' "%s/School/BYU/byuclasses' % (home))

# Clone research projects
call('git clone git@github.com:spencerlyon2/hbs.git' +
     ' "%s/Research/hbs' % (home))

call('git clone git@bitbucket.org:byumcl/yield-curve.git' +
     ' "%s/Research/yield-curve' % (home))

call('git clone git@bitbucket.org:byumcl/byumcl-python-tools.git' +
     ' "%s/Python/byumcl')

# Clone websites
call('git clone git@bitbucket.org:byumcl/byumcl.bitbucket.org.git' +
     ' "%s/Python/Sites/byumcl.bitbucket.org"' % (home))

call('git clone git@github.com:spencerlyon2/spencerlyon2.github.com.git' +
     ' "%s/Python/Sites/spencerlyon2"' % (home))
