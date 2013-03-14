import sublime_plugin
import os


class CurrentPathStatusCommand(sublime_plugin.EventListener):
    def updateStatus(self, view):
        dirty = view.is_dirty() and '**' or '--'
        eol = view.line_endings()[0]
        encoding = view.encoding()
        path = '%s%s%s' % (
            ' ' * 30,
            str(view.file_name() or '').replace(os.getenv('HOME'), '~'),
            ' ' * 30,
        )
        view.set_status(
            'cg_0',
            '%s%s%s' % (
                dirty,
                eol,
                '  [%s]  ' % encoding,
            ),
        )
    on_activated = updateStatus
    on_new = updateStatus
    on_clone = updateStatus
    on_load = updateStatus
    on_close = updateStatus
    on_pre_save = updateStatus
    on_post_save = updateStatus
    on_modified = updateStatus
    on_selection_modified = updateStatus
    on_activated_ = updateStatus
    on_deactivated = updateStatus
