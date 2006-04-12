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




from CoaSim.popStructure import Population as P, Merge as M, Sample as S, \
                                Bottleneck as B, Growth as G, \
                                Migration as Mi



arg = CoaSim.simulate([], P(1,M(1.5,[P(1,S(2),name='1'),P(1,S(2),name='2')])),
                          [Mi('1','2',0.001),Mi('2','1',0.002)],
                      keepEmptyIntervals=True, seed=10)

counter = dict()
for node in arg.nodes:
    try:
        counter[type(node)] += 1
    except KeyError:
        counter[type(node)] = 1

# no keepMigrationEvents so there should be no migration nodes
try:
    counter[CoaSim.MigrationNode]
    assert False
except KeyError:
    pass


arg = CoaSim.simulate([], P(1,M(1.5,[P(1,S(2),name='1'),P(1,S(2),name='2')])),
                          [Mi('1','2',1),Mi('2','1',2)],
                      keepEmptyIntervals=True, keepMigrationEvents=True,
                      seed=10)

counter = dict()
for node in arg.nodes:
    try:
        counter[type(node)] += 1
    except KeyError:
        counter[type(node)] = 1

# but now there should be!
# print counter[CoaSim.MigrationNode]
