# If You like this project - hit it with a star ツ

# 23.03.2013

---

Added option to define ``` osascript ``` path because some of you may have problem with PATHs.
From now by path to ``` osascript ``` it's set to ``` /usr/bin/osascript ``` and can be changed in settings.
More info in [issue #8](https://github.com/afterdesign/MacTerminal/issues/8).

For more info how to setup ``` osascript ``` path just checkout [README.md](https://github.com/afterdesign/MacTerminal)

Added "Open in terminal" to command palette.

---

# 30.01.2013

---

Some minor (print call) changes and MacTerminal is working with Sublime Text 3 beta !

Thanx to ap0 (https://github.com/ap0) there is default_path to fallback if there is a problem with file path.

There is also some interesting information from Thomas Noe 
about possible issue for people using dropbox for project syncing:
https://github.com/afterdesign/MacTerminal/issues/6#issuecomment-11831154

---



# 25.12.2012

---

Changed default keybinding to "ctrl+cmd+t" due to global bindings problems 
explained in [issue 5](https://github.com/afterdesign/MacTerminal/issues/5)

---

Added support for iTerm 2. Just go to:
    
```
Sublime Text 2 -> Preferences -> Package Settings -> Macterminal -> Settings - User
```

and add:

```
{
    "terminal"   :  "iterm"
}
```
If you wish you can also change it in Settings - Default.

---

Added full support for opening terminal from sidebar. 
Just right click on file or directory and use "Open in terminal".
For now there is no support for multiple selected files in sidebar.

---

Added simple FAQ on main project site. If you have any questions just 
ping me on [twitter](http://twitter.com/afterdeign) or 
simply write [issue on github](https://github.com/afterdesign/MacTerminal/issues).

---
