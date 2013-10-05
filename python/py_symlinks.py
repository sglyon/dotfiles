from os import symlink, getcwd
from os.path import expanduser, sep
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

this_dir = getcwd()
home = expanduser("~")

mplrc_dir = home + sep + ".matplotlib" + sep + "matplotlibrc"
ipy_dir = home + sep + ".ipython"

ln_mplrc = "ln -s ./matplotlibrc %s" % (mplrc_dir)

msg = "WARNING: Python doesn't correctly set the symlink for matplotlibrc\n"
msg += "To fix this paste the following in the terminal from this directory:\n"
msg += "\t" + ln_mplrc

print(msg + "\n")

attempt_symlink(this_dir + sep + "ipython", ipy_dir)
