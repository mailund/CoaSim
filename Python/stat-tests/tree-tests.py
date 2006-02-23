#!/bin/env python

from CoaSim import *
from math import sqrt

def E_treeHeight(n):
    return 2*(1-1.0/n)

def Var_treeHeigh(n):
    return 4*sum([1.0/(j**2*(j-1)**2) for j in xrange(2,n+1)])

def E_branchLength(n):
    return 2*sum([1.0/j for j in xrange(1,n)])

def Var_branchLength(n):
    return 4*sum([1.0/j**2 for j in xrange(1,n)])

def acceptedInterval(mean,var, noSamples):
    # Using the Central Limit Theorem we got that our average should
    # be ~N(mean,var/noSamples), so a 1% test gives us:
    return -2.33*sqrt(var/noSamples)+mean, 2.33*sqrt(var/noSamples)+mean



def meanTreeStats(n, noSamples):
    heights = [] ; lengths = []
    for i in xrange(noSamples):
        arg = simulate([],n,keepEmptyIntervals=True)
        tree = arg.intervals[0].tree
        heights.append(tree.height)
        lengths.append(tree.branchLength)
    return sum(heights)/noSamples, sum(lengths)/noSamples




noErrors = 0
def error(s):
    print s
    noErrors += 1

noSamples = 1000
for n in [2,5,10,20]:
    EH = E_treeHeight(n)
    VH = Var_treeHeigh(n)
    EBL = E_branchLength(n)
    VBL = Var_branchLength(n)
    
    MH, MBL = meanTreeStats(n,noSamples)

    low, high = acceptedInterval(EH,VH,noSamples)
    if not low <= MH <= high:
        exit('Unexpected mean tree height: %g [%g:%g]' % (meanHeight,low,high))

    low, high = acceptedInterval(EBL,VBL,noSamples)
    if not low <= MBL <= high:
        exit('Unexpected mean tree height: %g [%g:%g]' % (meanHeight,low,high))


if noErrors is not 0:
    import sys
    sys.exit(2)
