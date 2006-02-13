__doc__ = '''
CoaSim/Python -- Python bindings for Coasim
Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>

This module contains functions for making random marker
configurations.  '''

def randomPosition(seed=None):
    '''Draws a single random position, uniformly in in the interval
    (0,1(.'''
    if seed is not None:
        from random import seed as setSeed
        setSeed(seed)
    from random import uniform
    return uniform(0.0,1.0)

def makeRandomPositions(noPositions, seed=None):
    '''Make a list of noPositions random positions between 0
    (included) and 1 (excluded).

    The optional seed parameter enables you to draw positions with a
    fixed seed (i.e. deterministically) for testing purposes.'''
    if seed is not None:
        from random import seed as setSeed
        setSeed(seed)
    from random import uniform
    positions = []
    for i in xrange(noPositions):
        positions.append(uniform(0.0,1.0))
    positions.sort()
    return positions

def makeRandomSNPMarkers(noPositions, lowFreq, highFreq, seed=None):
    '''Make a list of (uniformly) randomly positioned SNP markers, all
    restricted to having the mutant frequency in the interval
    [lowFreq,highFreq].

    The optional seed parameter enables you to draw positions with a
    fixed seed (i.e. deterministically) for testing purposes.'''
    positions = makeRandomPositions(noPositions,seed)
    from Core import SNPMarker
    return [SNPMarker(p,lowFreq,highFreq) for p in positions]

def makeRandomTraitMarkers(noPositions, lowFreq, highFreq, seed=None):
    '''Make a list of (uniformly) randomly positioned trait markers, all
    restricted to having the mutant frequency in the interval
    [lowFreq,highFreq].

    The optional seed parameter enables you to draw positions with a
    fixed seed (i.e. deterministically) for testing purposes.'''
    positions = makeRandomPositions(noPositions,seed)
    from Core import TraitMarker
    return [TraitMarker(p,lowFreq,highFreq) for p in positions]

def makeRandomMSMarkers(noPositions, theta, K, seed=None):
    '''Make a list of (uniformly) randomly positioned (K-allele model)
    micro satellite markers, all with mutation rate theta and K
    alleles.

    The optional seed parameter enables you to draw positions with a
    fixed seed (i.e. deterministically) for testing purposes.'''
    positions = makeRandomPositions(noPositions,seed)
    from Core import MicroSatelliteMarker as MSMarker
    return [MSMarker(p,theta,K) for p in positions]
