#!/bin/env python

import CoaSim

markers = [CoaSim.SNPMarker(0.2, 0.1, 0.9), CoaSim.SNPMarker(0.3, 0.1, 0.9)]

intervals = CoaSim.simulate(markers, 5, rho=4, seed=10).intervals
trees = [i.tree for i in intervals]
assert intervals == [t.interval for t in trees]

# regression testing... FIXME: dependent on local random number generator...
assert [t.branchLength for t in trees] == [6.8694553479721083, 4.8932502412522147]
assert [t.height for t in trees] == [2.0935405459452552, 1.2403594539408294]

assert str(t) == "(('3' : 0.970517,'0' : 0.970517) : 0.269843,(('4' : 0.638217,'2' : 0.638217) : 0.165581,'1' : 0.803798) : 0.436561);"
