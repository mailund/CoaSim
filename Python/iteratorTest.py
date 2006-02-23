#!/bin/env python

import CoaSim
arg = CoaSim.simulate([], 5, rho=20, gamma=10, Q=2,
                      seed=10, keepEmptyIntervals=True)

counter = dict()
for node in arg.nodes:
    try:
        counter[type(node)] += 1
    except KeyError:
        counter[type(node)] = 1

assert len(counter) == 4
assert counter[CoaSim.LeafNode] == 5
assert counter[CoaSim.CoalescentNode] == 72
assert counter[CoaSim.RecombinationNode] == 88
assert counter[CoaSim.GeneConversionNode] == 82

