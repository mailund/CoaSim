#!/bin/env python

from CoaSim import *

cm = CustomMarker(0.1)
assert cm.position == 0.1

class MyMarker(CustomMarker):
    def __init__(self,pos):
        CustomMarker.__init__(self,pos)
    def defaultValue(self):
        return 1
    def mutate(self, parentAllele, edgeLength):
        return parentAllele+1

mm = MyMarker(0.5)
assert mm.position == 0.5
# regression test...
assert simulate([mm],5,seed=1).sequences == [[4], [5], [3], [5], [2]]


class NonMarker(object):
    pass

try:
    simulate([NonMarker()],2)
    assert False
except TypeError, e:
    assert str(e) == 'arg #1 contains a non-marker'

class Uninitialized(Marker): pass
try:
    simulate([Uninitialized()],2)
    assert False
except ValueError, e:
    assert str(e) == 'arg #1 contains an un-initialized marker'

class Uninitialized(CustomMarker):
    def __init__(self): pass
try:
    simulate([Uninitialized()],2)
    assert False
except ValueError, e:
    assert str(e) == 'arg #1 contains an un-initialized marker'



class MissingDefaultValue(CustomMarker):
    def __init__(self):
        CustomMarker.__init__(self,0.2)
try:
    simulate([MissingDefaultValue()],2)
    assert False
except AttributeError, e:
    assert str(e) == 'defaultValue'

class IncorrectDefaultValue(CustomMarker):
    def __init__(self):
        CustomMarker.__init__(self,0.2)
    def defaultValue(self, x):
        return 3
try:
    simulate([IncorrectDefaultValue()],2)
    assert False
except TypeError, e:
    pass

class IncorrectDefaultValue(CustomMarker):
    def __init__(self):
        CustomMarker.__init__(self,0.2)
    def defaultValue(self):
        return None
try:
    simulate([IncorrectDefaultValue()],2)
    assert False
except TypeError, e:
    assert str(e) == 'defaultValue() must return an integer.'



class MissingMutate(CustomMarker):
    def __init__(self):
        CustomMarker.__init__(self,0.2)
    def defaultValue(self): return 0
try:
    simulate([MissingMutate()],2)
    assert False
except AttributeError, e:
    assert str(e) == 'mutate'

class IncorrectMutate(CustomMarker):
    def __init__(self):
        CustomMarker.__init__(self,0.2)
    def defaultValue(self): return 0
    def mutate(self): return 0
try:
    simulate([IncorrectMutate()],2)
    assert False
except TypeError, e:
    pass

class IncorrectMutate(CustomMarker):
    def __init__(self):
        CustomMarker.__init__(self,0.2)
    def defaultValue(self): return 0
    def mutate(self,parentAllele,edgeLength): return ""
try:
    simulate([IncorrectMutate()],2)
    assert False
except TypeError, e:
    pass
