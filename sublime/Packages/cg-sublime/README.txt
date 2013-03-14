Current functions and keybindings

--

status_info.py

This adds a little dirty/clean indicator along with telling you the files current encoding and line ending style

--

super+alt+shift+left  : prev_group
super+alt+shift+right : next_group

This works like the similar view commands, but allows you to navigate your layout with your keyboard

--

super+down : forward_paragraph
super+up   : backward_paragraph

These move you to the nearest end or beginning of a block of text

--

super+alt+shift+c : clone_to_new_window

This opens the active file again in a new window

--

super+o : prompt_open_file_path

This replaces "open_file" with an emacs like prompt where you can type the path, it also has tab completion

--

super+shift+o : prompt_change_view

Lists all open "buffers" and when you select one, moves it front and center to your current group

--

shift+f5 : sort_paragraphs

Works like sort_lines but with paragraphs (blocks of text)
