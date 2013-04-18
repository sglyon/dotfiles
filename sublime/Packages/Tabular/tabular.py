"""
This plug-in is an attempt to reproduce vim tabular functionality in sublimetext

Features:
- align and unalign selection with regular expression
- adjust selection if it doesn't capture the whole lines
- don't align lines without delimiter inside if any are in selection
- use special format for more precise alignment
- multiple selection support

Author            : ask
Email             : a.skurihin@gmail.com
Version           : 0.6
Last modification : 31.10.2011
"""

from itertools import izip_longest
import re
from contextlib import contextmanager, nested
import sublime
import sublime_plugin

# TODO:
# think about saving originals indention


class selection:
    """Context manager witch automatically expands user's selection to lines
    and clear on exit. It returns dict with selected lines and region of it
    """

    def __init__(self, view):
        self.view = view

    def __enter__(self):
        regions = []
        for region in self.view.sel():
            region = self.expand_to_lines(region)
            selected_lines = self.get_selected_lines(region)
            regions.append({'selected_lines': selected_lines, 'region': region})
        return regions

    def get_selected_lines(self, region):
        return [self.view.substr(x) for x in self.view.lines(region)]

    def __exit__(self, *args):
        # TODO: handle errors here
        self.clear_selection()

    def expand_to_lines(self, region):
        """Adjust the selection to capture the whole lines.
        Doesn't expand if cursor is placed at first char of a next line.
        Returns region of expanded selection.
        """

        self.view.run_command('expand_selection', {'to': 'line'})
        empty_line_correction = region_text(self.view, region).split('\n').count('')
        region = sublime.Region(region.begin(), region.end()
                                - empty_line_correction)
        self.view.sel().clear()
        self.view.sel().add(region)
        return region

    def clear_selection(self):
        """Clear selection and place cursor in the last point of selection"""

        end = self.view.sel()[-1].end()
        self.view.sel().clear()
        self.view.sel().add(sublime.Region(end))


@contextmanager
def edit(view):
    """Context manager start and finish undo stage"""

    edit_sequence = view.begin_edit()
    yield edit_sequence
    view.end_edit(edit_sequence)


def region_text(view, region):
    return view.substr(region)


def join(to_join, delimiters):
    """Join elements of `to_join` by elements of `delimiters`
    Return joined string
    """

    for i, line in enumerate(to_join):
        for d in delimiters[i]:
            line[0:2] = [" {0} ".format(d).join((line[0:2]))]
    joined = [fst(x) for x in to_join]
    return joined


def align_region(text, delimiter, delimiters_to_use=None, format='l'):
    """Align text by `delimiter` using `format`
    #Argumens:
    text: list of strings
    delimiter: string, regexp
    delimiters_to_use: list, which delimiter to use by number
    format: string, aligment rule for columns
    """

    indention = get_indention(text)
    to_align_list = parse_delimiters_to_use(delimiters_to_use)
    splited_text, delimiters = re_split(delimiter, text, to_align_list)
    columns = swap(splited_text)
    columns_stripped = [strip(l) for l in columns]
    columns_with_format = zip_longest(columns_stripped, format.split(),
                                                        fillvalue='l')
    aligned = swap(align(*el) for el in columns_with_format)
    aligned_indented = filter_none([indent_first(indention, x)
                                              for x in aligned])
    to_insert = join(aligned_indented, delimiters)
    return '\n'.join(to_insert)


def align(column, format):
    """Align strings in lst with spaces by given format
    #Argumens:
    `format` - one of [c]enter [l]eft [r]ight (default=l)
    """
    if not column:
        return []
    max_str_len = len(max(column, key=len))
    aligned_columns = []
    for row in column:
        if not row:
            max_str_len = 0
        if format == 'l':
            aligned_row = row.ljust(max_str_len)
        elif format == 'r':
            aligned_row = row.rjust(max_str_len)
        elif format == 'c':
            aligned_row = row.center(max_str_len)
        aligned_columns.append(aligned_row)
    return aligned_columns


def re_split(delimiter, list_of_strings, split_to_use=False):
    """ Split `string` by re
    return tuple of splited strings and delimiters
    #Args:
    delimiter: regular expression, to split `string` by
    string: string to be splitted
    ##Optional:
    split_to_use: iterable, return only n-th spitted string
    by default return all splitted string
    """

    split = []
    delimiters = []
    for string in list_of_strings:
        i = 0
        cur_split = []
        cur_delimeters = []
        for index, match in enumerate(re.finditer(delimiter, string)):
            if not split_to_use or index in split_to_use:
                cur_delimeters.append(string[match.start():match.end()])
                cur_split.append(string[i:match.start()])
                i = match.end()
        cur_split.append(string[i:])
        split.append(cur_split)
        delimiters.append(cur_delimeters)
    return (split, delimiters)


def unalign_region(text, delimiter):
    """Unalign text by `delimiter` using `format`"""

    indention = get_indention(text)
    splited_text, delimiters = re_split(delimiter, text)
    unaligned = [strip(x) for x in splited_text]
    unaligned_indented = [indent_first(indention, x) for x in unaligned]
    to_insert = join(unaligned_indented, delimiters)
    return '\n'.join(to_insert)


def parse_delimiters_to_use(delimiters_to_use):
    if not delimiters_to_use:
        return []

    delimiters_to_use_re = "(?P<range>\d+:\d+)|(?P<list>\d+(,\d+)+)|(?P<digit>\d+$)"
    delimiters_to_use_parsed = re.match(delimiters_to_use_re, delimiters_to_use).groupdict()

    if delimiters_to_use_parsed['list']:
        choise = delimiters_to_use_parsed['list'].split(',')
    elif delimiters_to_use_parsed['range']:
        start, end = delimiters_to_use_parsed['range']
        choise = range(start, end)
    elif delimiters_to_use_parsed['digit']:
        choise = list(delimiters_to_use_parsed['digit'])
    choise = [int(x) - 1 for x in choise]

    return choise


def user_input_parser(string):
    """Parse user input.
    Return parsed tuple.
    """

    m = re.match('(?P<delimiter>[^/]+)/?(?P<delimiters_to_use>^[^0]\d+((,|:)\d+)?)?/?((?P<format>([lcr] ?)+))?',
                 string).groupdict()
    return (m[x] for x in m if m[x])


strip = lambda l: [s.strip() for s in l]
filter_none = lambda l: [filter(None, x) for x in l]
indent_first = lambda i, l: [i + l[0]] + l[1:]
get_indention = lambda l: (l[0])[0:len(l[0]) - len(l[0].lstrip())]
zip_longest = lambda *l, **kw: [x for x in izip_longest(*l, **kw)]
swap = lambda l: [list(x) for x in izip_longest(fillvalue='', *l)]
fst = lambda l: l[0]


class TabularizeCommand(sublime_plugin.TextCommand):

    def run(self, edit):
        self.view.window().show_input_panel('Align with [char]:', '',
                self.after_input, None, None)

    def after_input(self, user_input):
        if user_input and not self.view.sel()[0].empty():
            with nested(selection(self.view), edit(self.view)) as (sel, e):
                user_input_parsed = user_input_parser(user_input)
                for (i, reg) in enumerate(sel):
                    aligned_text = align_region(reg['selected_lines'],
                            *user_input_parsed)
                    self.view.replace(e, reg['region'], aligned_text)
                    try:
                        next_region = sel[i + 1]['region']
                        new_spaces_in_prev_reg = len(aligned_text) \
                            - len('\n'.join(reg['selected_lines']))
                        new_region = sublime.Region(next_region.begin()
                                + new_spaces_in_prev_reg, next_region.end()
                                + new_spaces_in_prev_reg)
                        sel[i + 1]['region'] = new_region
                    except IndexError:
                        pass


class UntabularizeCommand(TabularizeCommand):

    def run(self, edit):
        self.view.window().show_input_panel('Unalign with [char]:', '',
                self.after_input, None, None)

    def after_input(self, user_input):
        if user_input and not self.view.sel()[0].empty():
            with nested(selection(self.view), edit(self.view)) as (sel, e):
                for (i, reg) in enumerate(sel):
                    unaligned_text = unalign_region(reg['selected_lines'],
                            user_input)
                    self.view.replace(e, reg['region'], unaligned_text)
                    try:
                        next_region = sel[i + 1]['region']
                        new_spaces_in_prev_reg = len(unaligned_text) \
                            - len('\n'.join(reg['selected_lines']))
                        new_region = sublime.Region(next_region.begin()
                                + new_spaces_in_prev_reg, next_region.end()
                                + new_spaces_in_prev_reg)
                        sel[i + 1]['region'] = new_region
                    except IndexError:
                        pass
