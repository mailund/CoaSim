__doc__ = '''
CoaSim/Python -- Python bindings for Coasim
Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>

This module contains helper routines for input/output.  '''

def printPositions(positions, file=None):
    '''Prints positions to file (or stdout if file is not specified)
    as a list of space-separated numbers.

    If file is not None, it must be a file object that is open for
    writing; it is the callers responsibility to close the file object
    again.'''
    if file is None:
        import sys
        file = sys.stdout
    print >> file, ' '.join([str(p) for p in positions])

def printMarkerPositions(markers, file=None):
    '''Prints positions of markers to file (or stdout if file is not
    specified) as a list of space-separated numbers.

    If file is not None, it must be a file object that is open for
    writing; it is the callers responsibility to close the file object
    again.'''
    printPositions([m.position for m in markers], file)

def printSequences(sequences, file=None):
    '''Print a list of sequences to file (or stdout if file is not
    specified).  The sequences are output with a line of
    space-separated alleles for each sequence.

    If file is not None, it must be a file object that is open for
    writing; it is the callers responsibility to close the file object
    again.'''
    if file is None:
        import sys
        file = sys.stdout

    for seq in sequences:
        print >> file, ' '.join([str(a) for a in seq])
