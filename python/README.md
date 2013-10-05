Python Specialization
=====================

This directory contains files and folders that make up part of my python customization. As of right now, I settings for the following python packages here:

* `IPython`
* `matplotlib`

These files/folders need to be in a special place in order to work. to create symlinks from this folder to the proper destination, run `python py_symlinks.py` from this directory.

Notes
-----

I couldn't get python to properly create a symlink for the matplotlibrc file, but when the py_symlinks.py file is executed, it will print out a customized command that can simply be copied and pasted into the terminal to create the link.

The benefit of creating symlinks from here to the destination is that the config files can be kept with the rest of the my dotfiles under git control. Trying to make a symlink the other way around (from python suitable directory to here) makes it so they cannot be controlled by git (git doesn't deal with symlinks).
