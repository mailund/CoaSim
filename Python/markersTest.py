#!/bin/env python

from CoaSim import *

# testing legal input
m1 = SNPMarker(0.4, 0.1, 0.9)
assert m1.position == 0.4
assert m1.lowFreq  == 0.1
assert m1.highFreq == 0.9

m2 = TraitMarker(0.4, 0.1, 0.9)
assert m2.position == 0.4
assert m2.lowFreq  == 0.1
assert m2.highFreq == 0.9

m3 = MicroSatelliteMarker(0.4, 12.2, 10)
assert m3.position == 0.4
assert m3.theta    == 12.2
assert m3.K        == 10

m1.position = 0.5
assert m1.position == 0.5

# testing strings
assert str(m1) == 'SNPMarker(0.5,0.1,0.9)'
assert str(m2) == 'TraitMarker(0.4,0.1,0.9)'
assert str(m3) == 'MicroSatelliteMarker(0.4,12.2,10)'


# testing exceptions on illegal input
try:
    SNPMarker(1.4, 0.1, 0.9)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal position'

try:
    SNPMarker(0.4, 1.1, 0.9)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: 1.1'

try:
    SNPMarker(0.4, 0.1, 1.9)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: 1.9'

try:
    SNPMarker(0.4, 0.9, 0.1)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: low freq >= high freq.'


try:
    TraitMarker(1.4, 0.1, 0.9)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal position'

try:
    TraitMarker(0.4, 1.1, 0.9)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: 1.1'

try:
    TraitMarker(0.4, 0.1, 1.9)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: 1.9'

try:
    TraitMarker(0.4, 0.9, 0.1)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: low freq >= high freq.'



try:
    MicroSatelliteMarker(1.4, 12.3, 10)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal position'

try:
    MicroSatelliteMarker(0.4, -12.3, 10)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: -12.3'

try:
    MicroSatelliteMarker(0.4, 0, 10)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: 0'

try:
    MicroSatelliteMarker(0.4, 12.3, 0)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: 0'

try:
    MicroSatelliteMarker(0.4, 12.3, -10)
    assert False
except ValueError, e:
    assert str(e) == 'Illegal marker value: -10'


