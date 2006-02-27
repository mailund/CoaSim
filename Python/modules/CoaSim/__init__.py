__doc__ = '''
CoaSim/Python -- Python bindings for Coasim
Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>

CoaSim is a tool for simulating the coalescent process with
recombination and geneconversion under various demographic models. It
effectively constructs the ancestral recombination graph for a given
number of individuals and uses this to simulate samples of SNP,
micro-satellite, and other haplotypes/genotypes. The generated sample
can afterwards be separated in cases and controls, depending on states
of selected individual markers. The tool can accordingly also be used
to construct cases and control data sets for association studies.

To cite CoaSim, please use:

    CoaSim: A Flexible Environment for Simulating Genetic Data under
    Coalescent Models.  T. Mailund, M.H. Schierup, C.N.S. Pedersen,
    P.J.M. Mechlenborg, J.N. Madsen, and L. Schauser.
    BMC Bioinformatics 2005, 6:252. doi:10.1186/1471-2105-6-252.

CoaSim is developed in collaboration between Bioinformatics ApS and
Bioinformatics Research Center (BiRC) and released under the GNU
General Public License.

Please see http://www.daimi.au.dk/~mailund/CoaSim/index.html for more
details.'''

from CoaSim.Core import *


def simulate(markers, popStructSpec, migrationSpec=[],
             rho=0, Q=0, gamma=0, beta=0,
             callbacks=None,
             keepEmptyIntervals=False,
             keepMigrationEvents=False,
             seed=None):
    '''Simulate an Ancestral Recombination Graph (ARG).

    '''
    import CoaSim.Core as Core
    import popStructure
    if isinstance(popStructSpec,popStructure.Population):
        events,sampleSizes = popStructure.compile(popStructSpec,migrationSpec)
    elif isinstance(popStructSpec,int):
        if migrationSpec != []:
            raise ValueError("Migration specifications requires non-trivial population structure.")
        events,sampleSizes = [],[popStructSpec]
    else:
        raise TypeError('Population structure specification of unknown type.')

    # the Core library assumes a seed of 0 means no seed -- but here
    # we prefer the more conventional None, so we need to translate
    # it.
    if seed is None: seed = 0

    return Core.simulate(markers,sampleSizes,events,
                         rho,Q,gamma,beta,callbacks,
                         keepEmptyIntervals,keepMigrationEvents,
                         seed)


def isSorted(markers):
    '''A predicate that checks that markers are sorted with respect to
    their position (and no two markers are at the same position).'''
    for i in xrange(1,len(markers)):
        if markers[i-1].position >= markers[i].position:
            return False
    return True

def sortMarkers(markers):
    '''Sort a list of markers according to their positions.'''
    lst = [(m.position,m) for m in markers]
    lst.sort()
    return [x[1] for x in lst]

def _insertSorted(markers, newMarkers):
    '''Insert a list of newMarkers into a sorted list of markers,
    returning the new joined list and a list of indices for the
    positions the newMarkers where inserted in.  The newMarkers list
    need not be sorted, and the resulting list of indices will be in
    the same order as the positions the newMarkers got.'''
    assert isSorted(markers)

    # copy the two lists so we won't destroy them...
    markers = markers[:]
    newMarkers = newMarkers[:]

    # sort newMarkers so we can merge the two lists in linear time,
    # but remember the indices.
    newList = [(m.position,idx,m) \
               for idx,m in zip(range(len(newMarkers)),newMarkers)]
    newList.sort()
    newList = [(idx,m) for p,idx,m in newList]

    # now merge the two lists and remember the indices.  We start from
    # the back, since that is the easiest with Pythons pop() function.
    # Then we just have to reverse once we are done.
    indices = [None]*len(newMarkers)
    result = []
    idx = len(markers)+len(newMarkers)-1
    while markers != [] and newList != []:
        if markers[-1].position == newList[-1][1].position:
            raise ValueError('Two markers at the same position!')
        if markers[-1].position > newList[-1][1].position:
            m = markers.pop()
            result.append(m)
        else:
            i,m = newList.pop()
            result.append(m)
            indices[i] = idx
        idx -= 1

    # one of these two deals with the remaining markers
    while markers != []:
        result.append(markers.pop())
    while newList != []:
        i,m = newList.pop()
        result.append(m)
        indices[i] = idx
        idx -= 1
        
    # ...and remember to reverse the list...
    result.reverse()
    
    return result, indices


def insertSorted(markers, new):
    '''Insert a marker or a list of markers (new) into a sorted list
    of markers (markers), returning the new joined list and the index
    of the new marker(s).

    If new is a list of markers, they need not be sorted beforehand,
    and the resulting list of indices will be in the same order as the
    positions the new markers was.'''

    if isinstance(new,Marker):
        new = [new]
        markers, indices = _insertSorted(markers,new)
        return markers, indices[0]
    else:
        return _insertSorted(markers,new)









if __name__ == '__main__':
    markers = [SNPMarker(0.4,0,1),SNPMarker(0.2,0,1),SNPMarker(0.8,0,1)]
    assert not isSorted(markers)
    markers = sortMarkers(markers)
    assert [m.position for m in markers] == [0.2, 0.4, 0.8]
    assert isSorted(markers)

    new = [TraitMarker(0.3,0,1),TraitMarker(0.1,0,1),TraitMarker(0.5,0,1)]
    merged, indices = insertSorted(markers,new)
    assert [m.position for m in merged] == [0.1,0.2,0.3,0.4,0.5,0.8]
    assert indices == [2,0,4]

    merged, index = insertSorted(markers,TraitMarker(0.5,0,1))
    assert [m.position for m in merged] == [0.2,0.4,0.5,0.8]
    assert index == 2
