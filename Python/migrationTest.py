#!/bin/env python

import CoaSim
from CoaSim.popStructure import Population as P, Merge as M, Sample as S, \
                                Migration as Mi

popSpec = P(1,M(1.5,[P(1,S(4),name='1'),P(1,S(4),name='2')]))
migSpec = [Mi('1','2',1),Mi('2','1',1)]

tree = CoaSim.simulate([], popSpec, migSpec,
                       keepEmptyIntervals=True,
                       keepMigrationEvents=True,
                       seed=10).intervals[0].tree

assert str(tree) == "(((('5' : 0.259312,('0' : 0.171585,'3' : 0.171585) : [&migration={1,0.218047,0}] 0.0877268) : 0.13414,('2' : 0.218047,'1' : 0.218047) : [&migration={1,0.218047,0}] 0.175404) : 0.491754,('7' : 0.0821288,'4' : 0.0821288) : 0.803077) : 0.117671,'6' : [&migration={1,0.205103,0,0.171585,1}] 1.00288);"
