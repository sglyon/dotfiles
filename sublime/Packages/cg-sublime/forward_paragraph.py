import sublime
import sublime_plugin


def getCurrentSel(view):
    return view.sel()[0]


def getCurrentPoint(view):
    return getCurrentSel(view).a


class ForwardParagraph(sublime_plugin.TextCommand):
    '''
    Moves cursor to end of current paragraph
    '''
    backward = False

    def run(self, edit):
        v = self.view
        # if going backwards start back one line
        origPoint = getCurrentPoint(v)
        currentPoint = origPoint
        while self.backward and currentPoint > 0 and (v.substr(currentPoint) == '\n' or origPoint == currentPoint):
            currentLine = v.rowcol(currentPoint)[0]
            currentPoint = v.text_point(currentLine - 2, 0)
            v.sel().clear()
            v.sel().add(currentPoint)
            v.show(currentPoint)
        v.run_command("expand_selection_to_paragraph")
        currentSel = getCurrentSel(v)
        newPoint = currentSel.b
        if self.backward:
            newPoint = currentSel.a
        newRegion = sublime.Region(newPoint)
        v.sel().clear()
        v.sel().add(newRegion.a)
        v.show(newRegion.a)
        #if the next space is whitespace go till it's not or it's over
        currentPoint = getCurrentPoint(v)
        while v.substr(currentPoint) == '\n' and currentPoint <= v.size():
            currentPoint = currentPoint + 1
        v.sel().clear()
        v.sel().add(currentPoint)
        v.show(currentPoint)
