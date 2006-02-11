#!/bin/env python

import CoaSim

markers = [CoaSim.SNPMarker(0.2, 0.1, 0.9),
           CoaSim.SNPMarker(0.3, 0.1, 0.9)]

assert len(CoaSim.simulate(markers, 5).intervals) == 1 # no recomb, 1 interval

arg = CoaSim.simulate(markers, 5, rho=4)
assert arg.intervals == arg.intervals # check that comparison works
# this wont work, however: assert arg.intervals[0] is arg.intervals[0]
# since we get new objects for each call to intervals.

# regression testing... FIXME: dependent on local random number generator...
arg = CoaSim.simulate(markers, 5, rho=4, seed=10)
assert [i.start for i in arg.intervals] == [0.092338229572557953, 0.2567803758460937]
assert [i.end for i in arg.intervals] == [0.2458365472247063, 0.42215607660922971]
assert [i.length for i in arg.intervals] == [0.15349831765214833, 0.16537570076313601]

