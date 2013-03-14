Two days ago, looking for a possible [Eclipse][] replacement, I gave a try to [Sublime Text 2][].
One of my main usages of [PyDev][] (very good Eclipse plug-in for Python developer) is the code formatter.
Under ST2 I installed [PythonTidy][] but unfortunately it did not work for me.
So, for fun and learning, I decided to create a new ST2 plug-in: **Python PEP8 Autoformat**.
It is based on [autopep8][] as code formatter and [pep8][] as code linter.

# Python PEP8 Autoformat

Python PEP8 Autoformat is a Sublime Text 2 plug-in to interactively reformat Python source code according
to [PEP-8][] (Style Guide for Python Code). 

## Installation

To avoid dependencies, a version of autopep8 is shipped with this package. If you want to use version installed
on your system, you have to set up the path to autopep8 (see below).

1. Using [Sublime Package Control][]
    + Use `cmd+shift+P` shortcut then `Package Control: Install Package`
    + Look for `Python PEP8 Autoformat` and install it.

1. Using mercurial (hg) repository on bitbucket:
    + Open a terminal, cd to ST2/Packages directory (Preferences -> Browse packages). Then type in terminal:
    + `hg clone https://bitbucket.org/StephaneBunel/pythonpep8autoformat 'Python PEP8 Autoformat'`

1. Manually:
    + Download an [archive](https://bitbucket.org/StephaneBunel/pythonpep8autoformat/downloads)
      of Python PEP8 Autoformat
    + Change directory to ST2/Packages directory (Preferences -> Browse packages) and create a new
      directory named 'Python PEP8 Autoformat'
    + Extract archive contents in this 'Python PEP8 Autoformat' directory.


## Settings

You'll find settings in Preferences menu (Preferences -> Package Settings -> Python PEP8 Autoformat -> ...).

    {
        // autoformat code on save ?
        "autoformat_on_save": false,

        // select errors / warnings(e.g. ["E4", "W"])
        "select": [],

        // do not fix these errors / warnings(e.g. ["E501", E4", "W"])
        "ignore": [],

        // Maximum line length
        "max-line-length": 128,

        // enable possibly unsafe changes (E711, E712)
        "aggressive": false
    }

By editing User settings, your personal liking will be kept safe over plug-in upgrades.

## Usage

Formatting is applied on the whole document.

### Using keyboard:

- GNU/Linux: `ctrl+shift+r`
- OSX:       `ctrl+shift+r`
- Windows:   `ctrl+shift+r`

### Using Command Palette:

As defined in `Default.sublime-commands` file:

	[
	    { "caption": "User: Python PEP8 Autoformat", "command": "pep8_autoformat" }
	]

You can format your Python code by opening Command Palette (ctrl+shift+P)
and type "auto"... up to highlight full caption.

### Companions
Useful companions to Python PEP8 Autoformat:

+ [SublimeLinter][] - Inline lint highlighting
+ [MarkdownPreview][] - Markdown preview in browser

## License

Copyright 2012-2013 Stéphane Bunel

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


[PEP-8]:                   http://www.python.org/dev/peps/pep-0008/
[Sublime Text 2]:          http://www.sublimetext.com/
[autopep8]:                https://github.com/hhatto/autopep8
[pep8]:                    https://github.com/jcrocholl/pep8
[PythonTidy]:              https://github.com/witsch/SublimePythonTidy
[Eclipse]:                 http://www.eclipse.org/
[PyDev]:                   http://pydev.org/
[Sublime Package Control]: http://wbond.net/sublime_packages/package_control
[SublimeLinter]:           https://github.com/SublimeLinter/SublimeLinter
[MarkdownPreview]:         https://github.com/revolunet/sublimetext-markdown-preview
