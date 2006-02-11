#!/bin/env python

from CoaSim.randomMarkers import *

randWseed10 = [0.42888905467511462, 0.5714025946899135, 0.57809130113447038]
assert makeRandomPositions(3,10) == randWseed10

snps = makeRandomSNPMarkers(3,0.1,0.9,10)
positions = [m.position for m in snps]
lowFreqs  = [m.lowFreq  for m in snps]
highFreqs = [m.highFreq for m in snps]
assert positions == randWseed10
assert lowFreqs  == [0.1]*3
assert highFreqs == [0.9]*3

traits = makeRandomTraitMarkers(3,0.1,0.9,10)
positions = [m.position for m in traits]
lowFreqs  = [m.lowFreq  for m in traits]
highFreqs = [m.highFreq for m in traits]
assert positions == randWseed10
assert lowFreqs  == [0.1]*3
assert highFreqs == [0.9]*3

mss = makeRandomMSMarkers(3,12.3,10,10)
positions = [m.position for m in mss]
thetas    = [m.theta    for m in mss]
Ks        = [m.K        for m in mss]
assert positions == randWseed10
assert thetas    == [12.3]*3
assert Ks        == [10]*3

