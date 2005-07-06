
import sys
name = sys.argv[1]
file = open(sys.argv[2])

def quote(s):
    return s.replace('"', '\\"').replace('\n','\\n')

print '#define', name, '\\'
for line in file:
    print '"', quote(line), '"\\'
print '""'


