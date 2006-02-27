#!/bin/env python

import CoaSim

class callbacks(object):
    def __init__(self):
        self.counts = {'coa':0, 'rec':0, 'gc':0,
                       'bn_enter':0, 'bn_leave':0,
                       'g_enter':0, 'g_leave':0,
                       'mig':0, 'me': 0}

    def coalescentEvent(self, n, k):
        self.counts['coa'] += 1
    def recombinationEvent(self, n1, n2, k):
        self.counts['rec'] += 1
    def geneConversionEvent(self, n1, n2, k):
        self.counts['gc'] += 1

    def bottleneckEvent(self, pop, entering, t, k):
        if entering:
            self.counts['bn_enter'] += 1
        else:
            self.counts['bn_leave'] += 1
    def growthEvent(self, pop, entering, t, k):
        if entering:
            self.counts['g_enter'] += 1
        else:
            self.counts['g_leave'] += 1

    def migrationEvent(self, pop1, pop2, t, k):
        self.counts['mig'] += 1
    def mergeEvent(self, pops, t, k):
        self.counts['me'] += 1
        


from CoaSim.popStructure import Population as P, Merge as M, Sample as S, \
                                Bottleneck as B, Growth as G, \
                                Migration as Mi

cb = callbacks()
CoaSim.simulate([], P(1,M(1.5,[P(1,S(2),name='1'),P(1,S(2),name='2')]),
                      epochs=[B(.2,1.5,2),G(10,2)]),
                    [Mi('1','2',0.001),Mi('2','1',0.002)],
                rho=40,Q=10,gamma=2,
                keepEmptyIntervals=True, seed=10,
                callbacks=cb)

# this is just regression testing, not proper testing :-(
expected = {'me': 1, 'bn_leave': 1, 'mig': 4, 'g_enter': 1, 'coa': 173,
            'gc': 10, 'bn_enter': 1, 'g_leave': 0, 'rec': 168}
assert cb.counts == expected

class AllDone: pass
class ex_callbacks(callbacks):
    def coalescentEvent(self, n, k):
        callbacks.coalescentEvent(self, n, k)
        if self.counts['coa'] == 120:
            raise AllDone




cb = ex_callbacks()
try:
    CoaSim.simulate([], P(1,M(1.5,[P(1,S(2),name='foo'),P(1,S(2),name='bar')]),
                          epochs=[B(.2,1.5,2),G(10,2)]),
                    [Mi('foo','bar',0.001),Mi('bar','foo',0.002)],
                    rho=40,Q=10,gamma=2,
                    keepEmptyIntervals=True, seed=10,
                    callbacks=cb)
    assert False # we expect it to throw an exception
except AllDone:
    # this is just regression testing, not proper testing :-(
    expected = {'me': 0, 'bn_leave': 0, 'mig': 4, 'g_enter': 0, 'coa': 120,
                'gc': 6, 'bn_enter': 0, 'g_leave': 0, 'rec': 131}
    assert cb.counts == expected
