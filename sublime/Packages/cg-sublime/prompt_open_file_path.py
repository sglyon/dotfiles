import sublime
import sublime_plugin
from os import listdir
from os.path import commonprefix
from os.path import isdir
from os.path import exists
from os import getenv
from math import ceil
from math import floor

SCRATCH_COL_WIDTH = 40


class OpenWriteCommand(sublime_plugin.WindowCommand):

    def handle_open_write(self, write_file, use_scratch_buffer):
        self.scratch_file_list = None
        self.use_scratch_buffer = use_scratch_buffer

        currentDir = getenv('HOME') + '/'
        activeView = self.window.active_view()
        if activeView:
            currentFilePath = activeView.file_name()
            if currentFilePath:
                currentFileParts = currentFilePath.split('/')
                currentDir = '/'.join(
                    part for part in currentFileParts[:-1]) + '/'
        if write_file:
            promptText = "Write file:"
            doneCallback = self.on_done_write
        else:
            promptText = "Open file:"
            doneCallback = self.on_done_open
        self._ip = self.window.show_input_panel(
            promptText,
            currentDir,
            doneCallback,
            self.on_change,
            self.panel_was_closed
        )

    def on_change(self, text):
        if not text:
            return
        if text.startswith('\t') or text.endswith('\t'):
        #if text.endswith('\t'):
            currentFilePath = text.strip('\t')
            currentFileParts = currentFilePath.split('/')
            currentFile = currentFileParts[-1]
            currentDir = '/'.join(
                part for part in currentFileParts[:-1]) + '/'
            filesInDir = [
                fileName
                for fileName in listdir(currentDir)
                if fileName.startswith(currentFile)
            ]
            if filesInDir:
                if len(filesInDir) > 1:
                    if self.use_scratch_buffer:
                        self.set_scratch_file_list(filesInDir)
                    else:
                        statusText = ''.join((f + ', ') for f in filesInDir)
                        statusText = statusText[:-2]
                        statusText = '{ ' + statusText + ' }'
                        sublime.status_message(statusText)
                    newPath = currentDir + commonprefix(filesInDir)
                else:
                    newPath = currentDir + filesInDir[0]
            else:
                newPath = text[:-1]
                sublime.status_message(
                    'No files match "%s"' % currentFile)
            theEdit = self._ip.begin_edit()
            allTextRegion = self._ip.full_line(0)
            self._ip.replace(
                theEdit,
                allTextRegion,
                newPath + (
                    isdir(newPath) and not newPath.endswith('/') and '/' or '')
                )
            self._ip.end_edit(theEdit)

    def on_done_open(self, text):
        self.panel_was_closed()

        if not exists(text):
            # 'touch' file if it doesn't exist
            try:
                try:
                    f = open(text, 'w')
                finally:
                    f.close()
            except IOError:
                self.message('Unable to create file "[%s]"' % text)

        try:
            self.window.open_file(text)
            numGroups = self.window.num_groups()
            currentGroup = self.window.active_group()
            if currentGroup < numGroups - 1:
                newGroup = currentGroup + 1
            else:
                newGroup = 0
            self.window.run_command("move_to_group", {"group": newGroup})
        except:
            sublime.status_message('Unable to open "%s"' % text)

    def on_done_write(self, text):
        self.panel_was_closed()

        self.save_file_to_disk(text)

    def panel_was_closed(self):
        self.close_scratch_file_list_if_exists()

    def save_file_to_disk(self, file_name):
        window = self.window
        view = window.active_view()

        if view.is_dirty():
            view.set_scratch(True)

        file_contents = self.get_view_content()

        try:
            f = open(file_name, "w")
            try:
                f.write(file_contents)
            finally:
                f.close()
        except IOError:
            self.message('Unable to write file "[%s]"' % file_name)

        (group, index) = window.get_view_index(view)

        window.focus_view(view)
        window.run_command('close')
        try:
            new_view = window.open_file(file_name)
            window.set_view_index(new_view, group, index)
        except:
            sublime.status_message('Unable to open written file "%s"' % file_name)

    def get_view_content(self):
        view = self.window.active_view()

        # Get the default encoding from the settings
        encoding = view.encoding() if view.encoding() != 'Undefined' else 'UTF-8'

        # Get the correctly encoded contents of the view
        file_contents = view.substr(sublime.Region(0, view.size())).encode(encoding)
        return file_contents


    def set_scratch_file_list(self, files):
        if not self.scratch_file_list:
            # create scratch file list if it doesn't already exist
            self.scratch_file_list = self.window.new_file()
            self.scratch_file_list.set_scratch(True)
        else:
            # clear contents of existing scratch list
            self.scratch_file_list.set_read_only(False)
            edit = self.scratch_file_list.begin_edit()
            self.scratch_file_list.erase(edit, sublime.Region(0, self.scratch_file_list.size()))
            self.scratch_file_list.end_edit(edit)

        num_files = len(files)

        vp_extent = self.scratch_file_list.viewport_extent()
        view_height_chars = int(floor(vp_extent[1] / self.scratch_file_list.line_height()))
        view_width_chars = int(vp_extent[0] * self.scratch_file_list.em_width())

        num_rows = view_height_chars - 1
        num_cols = int(ceil(num_files / float(num_rows)))

        # make rows longer if the list of files won't fit on the screen horizontally
        if num_rows * SCRATCH_COL_WIDTH > view_width_chars:
            num_cols = view_width_chars / SCRATCH_COL_WIDTH
            num_rows = int(ceil(num_files / float(num_cols)))

        if num_files > 0:
            # pad last row with empty strings so it is exactly
            # divisible by num_cols
            num_rows_in_last_col = int(num_files % float(num_cols))
            if num_rows_in_last_col:
                num_rows_to_add = num_rows - num_rows_in_last_col
                rows_to_add = [u""] * num_rows_to_add
                files = files + rows_to_add

            # create string to display in buffer (multiple columns of text)
            buffer_text = u"%d files in directory, possible completions are:\n\n" % num_files

            split = [files[i:(i + num_files / num_cols)] for i in range(0, num_files, num_files / num_cols)]
            for row in zip(*split):
                buffer_text += u"".join(unicode.ljust(i, SCRATCH_COL_WIDTH) for i in row)
                buffer_text += u"\n"
        else:
            buffer_text = "No files found in current directory"

        edit = self.scratch_file_list.begin_edit()
        self.scratch_file_list.insert(edit, 0, buffer_text)
        self.scratch_file_list.end_edit(edit)

        self.scratch_file_list.set_read_only(True)

    def close_scratch_file_list_if_exists(self):
        if self.scratch_file_list:
            self.window.focus_view(self.scratch_file_list)
            if self.scratch_file_list.id() == self.window.active_view().id():
                self.window.run_command('close')


class PromptOpenFilePath(OpenWriteCommand):

    def run(self, use_scratch_buffer=True):
        self.handle_open_write(write_file=False, use_scratch_buffer=use_scratch_buffer)


class PromptWriteFilePath(OpenWriteCommand):

    def run(self, use_scratch_buffer=True):
        self.handle_open_write(write_file=True, use_scratch_buffer=use_scratch_buffer)

    

