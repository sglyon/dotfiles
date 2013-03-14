import sublime_plugin


class PromptChangeView(sublime_plugin.WindowCommand):

    def run(self):
        activeGroupIdx = self.window.active_group()
        numGroups = self.window.num_groups()
        fns = []

        curView = self.window.active_view()

        # get list of views by stack order
        self.allViewsByStackOrder = []
        self.allViewGroupsByStackOrder = []

        for zeroIdx in range(numGroups):
            # start processing active group, then all others
            idx = zeroIdx + activeGroupIdx
            if idx >= numGroups:
                idx -= numGroups

            self.window.focus_group(idx)

            numViews = len(self.window.views_in_group(idx))
            for i in range(numViews):
                viewToAdd = self.window.active_view()
                if viewToAdd.file_name() not in fns:
                    self.allViewsByStackOrder.append(viewToAdd)
                    self.allViewGroupsByStackOrder.append(idx)
                    fns.append(viewToAdd.file_name())
                self.window.run_command("next_view_in_stack")

        self.window.focus_view(curView)

        # remove active view
        fns = fns[1:]
        self.allViewsByStackOrder = self.allViewsByStackOrder[1:]
        self.allViewGroupsByStackOrder = self.allViewGroupsByStackOrder[1:]

        allViewFiles = [
            fn and '%s | %s' % (
                fn.split('/')[-1],
                fn.split('/')[-2],
            ) or "Unsaved File"
            for fn in fns
        ]

        self.window.show_quick_panel(
            allViewFiles,
            self.on_done,
        )

    def on_done(self, index):
        if index == -1:
            return
        newView = self.allViewsByStackOrder[index]
        theGroup = self.window.active_group()

        # clone view instead of moving it if it is the active view in another group
        cloneView = (self.window.active_view_in_group(self.allViewGroupsByStackOrder[index]).id() ==
                     newView.id())

        self.window.focus_view(newView)
        if not cloneView:
            self.window.run_command("move_to_group", {"group": theGroup})
        else:
            self.window.run_command('clone_file')
            self.window.run_command("move_to_group", {"group": theGroup})

