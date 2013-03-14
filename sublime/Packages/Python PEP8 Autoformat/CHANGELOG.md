# CHANGELOG
Python PEP8 Autoformat

## 2013.02.07-01
  - Quick and Dirty FIX to [multiline-selection-on-saving-formatting][issue9]: reformating on block selection is now disabled.
  Let me know if this feature was useful by opening a request to reintroduce it.

## 2013.01.26-02
  - Using the module API exposed by autopep8.

## 2013.01.26-01
  - FIX [Issue #7 View jumping in Sublime][issue7]. Thanks to Timon Wong
  - Update autopep8 to version 0.8.5.

## 2012.12.31-01
  - Add Side bar menu

## 2012.12.20-01
  - FIX [Issue #8 Autoformat on save][issue8]. Thanks to zhenyu li
  - Update autopep8 to version 0.8.4.

## 2012.11.24-01
 - Update autopep8 to version 0.8.3.
 - Add new setting "aggressive" (default false) to enable possibly unsafe changes (E711, E712)

## 2012.11.02-1
 - Update autopep8 to version 0.8.2. This version fix [W601][fix-github-issue40]
 - Add new setting "max-line-length" (default is 79).

## 2012.10.14-1
- FIX: [Issue #4][issue4] (format only one round).
- Upgrade shipped autopep8.
- Remove "command" and "show_command" settings since pep8 and autopep8 are bundled with plug-in
- autopep8: Some strange behavior persists when code contains non-ascii characters.
  Ex: [Fixing W601][fix-github-issue40] silently fails if source code contains non-ascii.

## 2012.09.29-1
- FIX: [Issue #3][issue3]. ST2 pep8 version issue on OS X.

## 2012.09.22-1
- Upgrade autopep8 utility to version 0.8.1.

## 2012.09.16-1
- FIX: Issue #2 raises another issue from autopep8 and Unicode, see [autopep8 issue #40][autopep8-issue40].
Now uses a fixed autopep8 from [hhatto/fix-github-issue40][fix-github-issue40] branch.
- Upgrade shipped lib2to3 to version 2.6.8

## 2012.09.14-1
- FIX: [Issue #2][issue2]. Include lib2to3 in package (Jim Wallace).

## 2012.09.10-1
- FIX: Enhance python code recognition

## 2012.09.07-1
- autopep8 updated to version 0.8

## 2012.08.02-1
- Python PEP8 Autoformat is now installable via [Sublime Package Control][spp]

## 2012.07.22-1
- Cosmetic code changes
- Tested under Windows 7
- Cleanup README.md (thanks to Georges Goncalves)

## 2012.07.21-2
- README update

## 2012.07.21-2
- FIX: OSX key binding is now `ctrl+shift+r`

## 2012.07.21-1
- NEW: Settings menu (Preferences -> Packages Settings -> Python PEP8 Autoformat)
- NEW: Include pep8 v1.3.3 and autopep8 v0.7.2 (with minor modifications) to remove all dependencies.
- FIX: variable reference

## 2012.07.19-1
- FIX: Restore caret position after reformatting the whole document.
- NEW: This file (CHANGELOG.md).

## 2012.07.18-1
- ADD: `Default.sublime-commands` file to call "Python PEP8 Autoformat" from Command Panel.
- ADD: `pep8_autoformat.sublime-settings` settings file:

		{
			// autoformat code on save ?
			"autoformat_on_save": false,

		    // path to autopep8 or "" to use packaged version
		    "command": "",

		    // select errors / warnings(e.g. ["E4", "W"])
		    "select": [],

		    // do not fix these errors / warnings(e.g. ["E4", "W"])
		    "ignore": ["E501"],

		    // for debugging purpose (print executed command in console)
		    "show_command": false
		}

## 2012.07.17-1
- Source code published on [bitbucket][sources].

## 2012.07.15-1
- Learning how to create a plug-in for Sublime text 2,
- and write the first lines of code.


[sources]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat
[spp]: http://wbond.net/sublime_packages/package_control
[issue2]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/2/import-error-during-formatting
[autopep8-issue40]: https://github.com/hhatto/autopep8/issues/40
[fix-github-issue40]: https://github.com/hhatto/autopep8/issues/40
[issue3]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/3/downloaded-and-getting-error-on-ctrl-shift
[issue4]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/4/format-only-one-round
[issue7]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/7/view-jumping-in-sublime
[issue8]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/8/change-on_post_save-to-on_pre_save
[issue9]: https://bitbucket.org/StephaneBunel/pythonpep8autoformat/issue/9/multiline-selection-on-saving-formatting
