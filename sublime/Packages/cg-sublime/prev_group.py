import sublime_plugin


class PrevGroup(sublime_plugin.WindowCommand):

    def run(self):
        numGroups = self.window.num_groups()
        currentGroup = self.window.active_group()
        if currentGroup > 0:
            newGroup = currentGroup - 1
        else:
            newGroup = numGroups - 1
        self.window.focus_group(newGroup)
