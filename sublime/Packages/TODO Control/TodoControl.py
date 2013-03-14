import sublime, sublime_plugin
import itertools

version = "1.5"

### settings: ###

settings_name = "TodoControl.sublime-settings"
settings = sublime.load_settings(settings_name)

POSTFIX = None
PREFIX = None
TAGS = None
COLORS = None
ID = -1

def on_tags_change():
	global TAGS
	TAGS = map(lambda t: "%s%s%s" % (PREFIX, t, POSTFIX), settings.get("tags",[]))

def on_colors_change():
	global COLORS
	COLORS = settings.get("colors",{})

def on_postfix_change():
	global POSTFIX
	POSTFIX = settings.get("postfix","")

def on_prefix_change():
	global PREFIX
	PREFIX = settings.get("prefix","")

on_prefix_change()
on_postfix_change()
on_tags_change()
on_colors_change()

settings.add_on_change("prefix", on_prefix_change)
settings.add_on_change("postfix", on_postfix_change)
settings.add_on_change("tags", on_tags_change)
settings.add_on_change("colors", on_colors_change)

##################

IsWIN = sublime.platform() == "windows"

line_ending = "\n"
if IsWIN:
	line_ending = "\r\n"

##################

def join_lists(ls):
	return list(itertools.chain.from_iterable(a))

def write_file(fl, s):
	f = open(fl, 'w')
	f.write(s)
	f.close()

def read_file(fl):
	f = open(fl, 'r')
	res = f.read()
	f.close()
	return res

def file_lines(fl):
	f = open(fl, 'r')
	res = f.readlines()
	f.close()
	return res

def unlines(lst):
	return line_ending.join(lst)

### tags ###

def get_tags(ss, tags):
	i,s = ss
	for t in tags:
		n = s.find(t)
		if n != -1:
			return (i, s[n:])
	return None

# get all tags from the text in the firm (line number, tag)
def get_tags_text(lines, tags):
	return filter(lambda s: s != None, map(lambda ss: get_tags(ss,tags), enumerate(lines) ) )

def format_lines_tags(lines, tags):
	return map(lambda (i, s): "line :%d: %s" % (i, s), get_tags_text(lines, tags) )

def get_tags_file(name, tags):
	#return format_lines_tags(name, 0, file_lines(name), tags)
	return format_lines_tags(file_lines(name), tags)

def get_tags_files(names, tags):
	return join_lists(map(lambda name: get_tags_file(name, tags), names) )

def get_tags_view(view, tags):
	reg = sublime.Region(0,view.size())
	lines = map(view.substr, view.split_by_newlines(reg))
	#return format_lines_tags(view.file_name(), view.id(), lines, tags)
	return format_lines_tags(lines, tags)

############

### colors ###

def patch_scheme():
	pass

def unpatch_scheme():
	pass

def view_name(view):
	name = view.file_name()
	if name == None:
		name = str(view.id())
	return name

##############

# TODO: hi!

# console output of current view's tags
class PrintTagsCommand(sublime_plugin.TextCommand):
	def run(self, edit):
		r = get_tags_view(self.view, TAGS)
		if r != []:
			print "In file: %s:\n%s\n" % (view_name(self.view), unlines(r))

# console output of current view's tags
class PrintAllTagsCommand(sublime_plugin.TextCommand):
	def run(self, edit):
		res = ""
		for wnd in sublime.windows():
			for view in wnd.views():
				if view.id() != ID:
					r = get_tags_view(view, TAGS)
					if r != []:
						res += "In file: %s:\n%s\n" % (view_name(view), unlines(r))
		print res

# gui output of current view's tags (in new view)
class LogTagsCommand(sublime_plugin.WindowCommand):
	def run(self):
		global ID
		v = self.window.active_view()
		res = get_tags_view(v, TAGS)
		if res == []:
			return
		s = "In file: %s:\n%s\n" % (view_name(v), unlines(res))
		view = self.window.new_file()
		ID = view.id()
		edit = view.begin_edit()
		view.insert(edit, 0, s)
		view.end_edit(edit)
		self.window.focus_view(view)
# TODO: nya
# gui output of current view's tags (in new view)
class LogAllTagsCommand(sublime_plugin.WindowCommand):
	def run(self):
		global ID
		res = ""
		for wnd in sublime.windows():
			for view in wnd.views():
				if view.id() != ID:
					r = get_tags_view(view, TAGS)
					if r != []:
						res += "In file: %s:\n%s\n" % (view_name(view), unlines(r))
		if res == "":
			return
		view = None
		if ID == -1:
			view = self.window.new_file()
			ID = view.id()
			edit = view.begin_edit()
			view.insert(edit, 0, res)
		else:
			for wnd in sublime.windows():
				for v in wnd.views():
					if v.id() == ID:
						view = v
						break
			edit = view.begin_edit()
			view.replace(edit, sublime.Region(0, view.size()), res)
		view.end_edit(edit)
		self.window.focus_view(view)


# gui output of current view's tags (in new view)
class EnterPressedCommand(sublime_plugin.TextCommand):
	def run(self, edit):
		if ID == -1 or self.view.id() != ID:
			return
		vi = self.view
		line = vi.substr(vi.line(vi.sel()[0]))

		line = line[line.find(":")+1:]
		num = int(line[:line.find(":")])

		row, col = vi.rowcol(vi.sel()[0].begin())
		while vi.substr(vi.line(vi.text_point(row,0)))[0:2] != "In":
			row -= 1
		name = vi.substr(vi.line(vi.text_point(row,0)))[9:-1]

		print name

		id = -1
		try:
			id = int(name)
		except ValueError:
			pass
		if id != -1:
			for wnd in sublime.windows():
				for view in wnd.views():
					if view.id() == id:
						wnd.focus_view(view)
						view.show_at_center(view.text_point(num,0))
						view.sel().clear()
						view.sel().add(view.line(view.text_point(num, 0)))
						return
		else:	
			for wnd in sublime.windows():
				for view in wnd.views():
					if view.file_name() == name:
						wnd.focus_view(view)
						view.show_at_center(view.text_point(num,0))
						view.sel().clear()
						view.sel().add(view.line(view.text_point(num, 0)))
						return

# TODO: ololo

class Listener(sublime_plugin.EventListener):
	# deinit current id
	def on_close(self, view):
		global ID
		if view.id() == ID:
			ID = -1

