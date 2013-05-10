# -*- coding: utf-8 -*-
'''
Sublime text plugin that opens terminal.
'''

import sublime_plugin
import os
import sublime
import subprocess

class OpenMacTerminal(sublime_plugin.TextCommand):#pylint: disable-msg=R0903,W0232
    '''
    Class is opening new terminal window with the path of current file  
    '''

    def run(self, edit, paths = None):#pylint: disable-msg=W0613
        '''
        Sublime text run
        
        @param edit: sublime.Edit
        @param paths: paths from sidebar
        '''

        #get settings
        settings = sublime.load_settings('MacTerminal.sublime-settings')
        terminal_name = settings.get("terminal")
        default_path = settings.get("default-path")
        
        if len(terminal_name) == 0:
            return

        # set command to run applescript
        command = []

        # get osascript from settings or just use default value
        command.append(settings.get("osascript") or "/usr/bin/osascript")

        # set path and terminal
        applescript_path = "{packages_dir}/MacTerminal/macterminal_{terminal_name}.scpt".format(
            packages_dir = sublime.packages_path(),
            terminal_name = settings.get("terminal")
            )
        
        command.append(applescript_path)

        #add path
        if paths is not None and len(paths) == 1:
            command.append(paths[0])#pylint: disable-msg=E1101
        elif self.view.file_name() is not None:
            command.append(os.path.dirname(self.view.file_name()))#pylint: disable-msg=E1101
        elif self.view.window().active_view().file_name() is not None:
            command.append(os.path.dirname(self.view.window().active_view().file_name()))#pylint: disable-msg=E1101
        elif len(default_path) > 0:
            exp_path = os.path.normpath(os.path.expanduser(default_path))
            command.append(exp_path)
        else:
            print("This may be a bug, please create issue on github")

        #open terminal
        subprocess.Popen(command)#pylint: disable-msg=E1101