import sublime
import sublime_plugin


class CloneToNewWindow(sublime_plugin.WindowCommand):
    def run(self):
        self.window.run_command("new_window")
        sublime.windows()[-1:][0].open_file(self.window.active_view().file_name())
