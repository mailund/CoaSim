#!/bin/env python

import CoaSim

markers = [CoaSim.SNPMarker(0.2, 0.1, 0.9),
           CoaSim.SNPMarker(0.3, 0.1, 0.9)]

# first a very simple regression test ... not really thorough
seqs = CoaSim.simulate(markers, 5, rho=40, seed=10).sequences
assert seqs == [[0, 0], [0, 0], [1, 0], [0, 0], [0, 1]]

# test incorrect input
try:
    CoaSim.simulate([markers[1],markers[0]], 5)
    assert False
except ValueError, e:
    assert str(e) == 'Marker positions out of sequence.'

try:
    CoaSim.simulate(markers, -5)
    assert False
except ValueError, e:
    assert str(e) == 'Non-positive sample size.'


try:
    CoaSim.simulate(markers, 5, rho=-2)
    assert False
except ValueError, e:
    assert str(e) == 'Negative rate or intensity: -2.'

try:
    CoaSim.simulate(markers, 5, gamma=-2)
    assert False
except ValueError, e:
    assert str(e) == 'Negative rate or intensity: -2.'

try:
    CoaSim.simulate(markers, 5, Q=-2)
    assert False
except ValueError, e:
    assert str(e) == 'Negative rate or intensity: -2.'

try:
    CoaSim.simulate(markers, 5, beta=-2)
    assert False
except ValueError, e:
    assert str(e) == 'Negative rate or intensity: -2.'


from CoaSim.popStructure import Population as P, Merge as M, Sample as S
CoaSim.simulate(markers, P(1,M(1.5,[P(1,S(2)),P(1,S(2))])))

from CoaSim.popStructure import Bottleneck as B, Growth as G
CoaSim.simulate(markers, P(1,M(1.5,[P(1,S(2)),P(1,S(2))]),epochs=[B(.2,1.5,2)]))
CoaSim.simulate(markers, P(1,M(1.5,[P(1,S(2)),P(1,S(2))]),epochs=[G(20,1.5,2)]))
CoaSim.simulate(markers, P(1,M(1.5,[P(1,S(2)),P(1,S(2))]),epochs=[B(.2,1.5)]))

from CoaSim.popStructure import Migration as Mi
CoaSim.simulate(markers,
                P(1,M(1.5,[P(1,S(2),name='foo'),P(1,S(2),name='bar')])),
                [Mi('foo','bar',0.1),Mi('bar','foo',0.2)])
