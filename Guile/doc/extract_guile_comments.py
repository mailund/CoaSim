#!/bin/env python

import re

_guile_comment_start = re.compile(r'.*<GUILE COMMENT>')
_guile_comment_end   = re.compile(r'.*</GUILE COMMENT>')

_cpp_comment_start    = re.compile(r'^[ \t]*\*')
_scheme_comment_start = re.compile(r'^[ \t]*;+')

xml_header = '''<?xml version="1.0" encoding="ISO-8859-1" ?>
<?xml-stylesheet type="text/xsl" href="guile-bindings.xsl"?>
'''


def wrap_comments(comments):
    return xml_header+'\n<guile-bindings>\n'+comments+'\n</guile-bindings>\n'


def extract_comments_from_file(f):
    in_comment = False
    comments = []
    for line in f:
        if not in_comment:
            if _guile_comment_start.match(line): in_comment = True
        else:
            if _guile_comment_end.match(line): in_comment = False
            else:
                # get rid of comments at the beginning of the string
                line = _cpp_comment_start.sub('', line)
                line = _scheme_comment_start.sub('', line)
                comments.append(line)
                
    return ''.join(comments)

  

if __name__ == '__main__':
    import sys
    docs = []
    for fname in sys.argv[1:]:
        f = open(fname)
        docs.append(extract_comments_from_file(f))
    print wrap_comments('\n'.join(docs))
