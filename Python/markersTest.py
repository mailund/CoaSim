#!/bin/env python

from CoaSim import *

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
