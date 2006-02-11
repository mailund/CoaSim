#!/bin/env python

import CoaSim

markers = [CoaSim.SNPMarker(0.2, 0.1, 0.9),
           CoaSim.SNPMarker(0.3, 0.1, 0.9)]

intervals = CoaSim.simulate(markers, 5, rho=4, seed=10).intervals

i1 = intervals[0]
t1 = i1.tree
r  = t1.root

assert r.isAncestral(i1.start)
assert not r.isAncestral(i1.end)
assert r.isAncestral(i1.start + (i1.end-i1.start)/2)
assert not r.isAncestral(i1.start - 1e-10)
assert r.isAncestral(i1.end - 1e-10)

assert r.children == r.children         # testing comparison...

# regression testing...
assert len(r.children) == 2
assert len(r.children[0].children) == 1
