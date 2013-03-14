# sublime imports
import sublime
import sublime_plugin

# python imports
import re

# globals
newlines = re.compile("\n{2,}")


class SortParagraphs(sublime_plugin.TextCommand):
    '''
    Sort paragraphs (similar to emacs sort-paragraphs)
    '''

    def run(self, edit):
        v = self.view
        curSel = v.sel()
        if len(curSel) > 1:
            sublime.error_message("Can't sort mutiple selections")
            return
        curText = v.substr(curSel[0])
        paragraphs = newlines.split(curText.strip())
        paragraphs.sort()
        v.replace(edit, curSel[0], '\n\n'.join(paragraphs) + '\n')
        sublime.status_message("%s paragraphs sorted" % len(paragraphs))
