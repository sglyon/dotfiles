LaTeX-Track-Changes
===================

Overview
---------

Please note, ST2 and LaTeX both use the word "package" to define their extension components. So, the ST2 package is the one you're reading about now, while the LaTeX Track Changes package is the one you need to download / read about from http://trackchanges.sourceforge.net/.

This Sublime Text 2 package provides snippets useful while using the LaTeX Track Changes package.

All track changes commands are supported:


\\note[editor]{The note}

\\annote[editor]{Text to annotate}{The note}

\\add[editor]{Text to add}

\\remove[editor]{Text to remove}

\\change[editor]{Text to remove}{Text to add} 



Installation
------------
1. Clone or copy this repository into:

- OS X: ~/Library/Application Support/Sublime Text 2/Packages/
- Windows: %APPDATA%/Sublime Text 2/Packages/
- Linux: ~/.config/sublime-text-2/Packages/

Usage
-------

This will not work unless you are in a LaTeX document.

When a snippet is needed, type Ctr+Shift+P and then type the desired track changes command from the available ones.

Example, to add text, press Ctr+Shift+P then type add, then select the line that displays "Snipped: add", and then navigate with TAB through the two fields, i.e. "editor" and "Text to add".