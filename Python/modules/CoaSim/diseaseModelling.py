__doc__ = '''
CoaSim/Python -- Python bindings for Coasim
Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>

This module contains functions for split sequences into affected and
unaffected individuals, based on various disease models.
'''

from random import uniform as _uniform

def _prj(indices, lst):
    '''Extract values in indices in lst (a projection to those dimensions).'''
    return [lst[i] for i in indices]

def _cut(indices, lst):
    '''Remove indices from lst and create a new list with the
    remaining values.'''
    return [lst[i] for i in xrange(len(lst)) if i not in indices]

def _splitSequences(predicate, indices, sequences, keepIndices):
    '''Split the list of sequences into two lists, the first list
    containing the sequences where predicate evaluates to True the
    other the sequences where predicate evaluates to False.  The
    function predicate is called with the values indexed by indices
    the individual sequences only, not the full sequences.  Unless
    keepIndices is set to True, the indices passed to predicate is
    removed from the resulting lists.'''

    if keepIndices: c = lambda i,x: x
    else:           c = _cut

    isTrue  = [] ; isFalse = []
    for seq in sequences:
        if predicate(*_prj(indices,seq)): isTrue.append(c(indices,seq))
        else:                             isFalse.append(c(indices,seq))

    return (isTrue,isFalse)

def _haploidToDiploid(sequences):
    '''Translates an even number of haploid sequences into a list of
    diploid sequences by combining them pairwise.'''
    assert len(sequences) % 2 == 0
    result = []
    for i in xrange(len(sequences)/2):
        h1 = sequences[2*i]
        h2 = sequences[2*i+1]
        result.append(zip(h1,h2))
    return result

def _diploidToHaploid(sequences):
    '''Translates a list of diploid sequences into a list of twice as
    many haploid sequences.'''
    result = []
    for s in sequences:
        result.append([x[0] for x in s])
        result.append([x[1] for x in s])
    return result

class DiseaseModel(object):
    '''Abstract class for disease modelling.'''

    HAPLOTYPE_MODEL = 1
    GENOTYPE_MODEL = 2

    def __init__(self, indices, model=HAPLOTYPE_MODEL, predicate=None):
        '''Creates a disease model where the indices determines the
        disease affecting markers and the mode is either
        HAPLOTYPE_MODEL (for haploid data) or GENOTYPE_MODEL (for
        diploid data).

        If predicate is given, it must be a function that determines
        disease status (returns True or False), dependent on the
        alleles (or pairs of alleles for GENOTYPE_MODEL) passed to it
        for each sequence.'''
        if isinstance(indices,int): self.indices = [indices]
        else:                       self.indices = indices
        self.model = model
        self.predicate = predicate

    def __call__(self, *args):
        if self.predicate is not None:
            return self.predicate(*args)
        raise NotImplementedError()


def singleMarkerDisease(markerIndex,
                        wildTypeRisk=0.0, mutantRisk=1.0,
                        homozygoteWildTypeRisk=0.0,
                        heterozygoteRisk=0.5,
                        homozygoteMutantRisk=1.0,
                        model=DiseaseModel.HAPLOTYPE_MODEL):
    '''Builds a simple disease model, where only a single marker
    affects the disease status, and where 0 assumed to be the
    wild-type allele and any other allele a mutant allele (all with
    the same disease risk).

    The disease affecting marker is given by markerIndex, and where
    the wild-type and mutant allelic risks are given by probabilities
    of disease.

    If the sequences are haploid (model is
    DiseaseModel.HAPLOTYPE_MODEL), the probabilities wildTypeRisk and
    mutantRisk are used; if the sequences are diploid (model is
    DiseaseModel.GENOTYPE_MODEL), the probabilities
    homozygoteWildTypeRisk, heterozygoteRisk, and homozygoteMutantRisk
    are used.
    '''
    class Model(DiseaseModel):
        def __init__(self):
            DiseaseModel.__init__(self,[markerIndex],model)
        if model == DiseaseModel.HAPLOTYPE_MODEL:
            def __call__(self, allele): 
                if allele==0: return _uniform(0.0,1.0) < wildTypeRisk
                else:         return _uniform(0.0,1.0) < mutantRisk
        else:
            def __call__(self, (a1,a2)):
                if (a1,a2) == (0,0):
                    return _uniform(0.0,1.0) < homozygoteWildTypeRisk
                elif a1 == 0 or a2 == 0:
                    return _uniform(0.0,1.0) < heterozygoteRisk
                else:
                    return _uniform(0.0,1.0) < homozygoteMutantRisk
    return Model()

def dominantModel(markerIndex):
    '''Builds a dominant diploid disease model for a single marker.'''
    return singleMarkerDisease(markerIndex,
                               model=DiseaseModel.GENOTYPE_MODEL,
                               homozygoteWildTypeRisk=0,
                               heterozygoteRisk=1,
                               homozygoteMutantRisk=1)

def recessiveModel(markerIndex):
    '''Builds a recessive diploid disease model for a single marker.'''
    return singleMarkerDisease(markerIndex,
                               model=DiseaseModel.GENOTYPE_MODEL,
                               homozygoteWildTypeRisk=0,
                               heterozygoteRisk=0,
                               homozygoteMutantRisk=1)

def split(diseaseModel, sequences, keepIndices=False):
    '''Split the list of sequences into two lists, based on the
    disease model, the first list consisting of affected individuals,
    the second on unaffected.

    Unless keepIndices is set to True, the disease affecting markers,
    as determined by diseaseModel, are removed from the resulting
    sequences.'''
    if diseaseModel.model == DiseaseModel.GENOTYPE_MODEL:
        diploid = _haploidToDiploid(sequences)
        cases,controls = _splitSequences(diseaseModel, diseaseModel.indices,
                                         diploid, keepIndices)
        return _diploidToHaploid(cases),_diploidToHaploid(controls)
    else:
        return _splitSequences(diseaseModel, diseaseModel.indices,
                               sequences, keepIndices)



if __name__ == '__main__':
    lst = range(10)
    assert _prj((0,2,4),lst) == [0,2,4]
    assert _cut((0,2,4),lst) == [1,3,5,6,7,8,9]

    seqs = [[0,0,1,1], [1,0,1,1], [0,0,0,0], [1,0,0,0]]
    lst1,lst2 = _splitSequences(lambda a0: a0==1, [0], seqs, True)
    assert lst1 == [seqs[1],seqs[3]]
    assert lst2 == [seqs[0],seqs[2]]
    lst1,lst2 = _splitSequences(lambda a0: a0==1, [0], seqs, False)
    assert lst1 == [_cut([0],seqs[1]),_cut([0],seqs[3])]
    assert lst2 == [_cut([0],seqs[0]),_cut([0],seqs[2])]

    lst1,lst2 = _splitSequences(lambda a0,a2: a0==a2, (0,2), seqs, True)
    assert lst1 == [seqs[1],seqs[2]]
    assert lst2 == [seqs[0],seqs[3]]
    lst1,lst2 = _splitSequences(lambda a0,a2: a0==a2, (0,2), seqs, False)
    assert lst1 == [_cut((0,2),seqs[1]),_cut((0,2),seqs[2])]
    assert lst2 == [_cut((0,2),seqs[0]),_cut((0,2),seqs[3])]

    assert _haploidToDiploid(seqs) == [[(0,1), (0,0), (1,1), (1,1)],
                                       [(0,1), (0,0), (0,0), (0,0)]]
    assert seqs == _diploidToHaploid(_haploidToDiploid(seqs))

    model = singleMarkerDisease(0)
    lst1,lst2 = _splitSequences(model, [0], seqs, True)
    assert lst1 == [seqs[1],seqs[3]]
    assert lst2 == [seqs[0],seqs[2]]
    lst1,lst2 = _splitSequences(model, [0], seqs, False)
    assert lst1 == [_cut([0],seqs[1]),_cut([0],seqs[3])]
    assert lst2 == [_cut([0],seqs[0]),_cut([0],seqs[2])]
    
    lst1,lst2 = split(model, seqs, True)
    assert lst1 == [seqs[1],seqs[3]]
    assert lst2 == [seqs[0],seqs[2]]
    lst1,lst2 = split(model, seqs)
    assert lst1 == [_cut([0],seqs[1]),_cut([0],seqs[3])]
    assert lst2 == [_cut([0],seqs[0]),_cut([0],seqs[2])]
    
    lst1,lst2 = split(dominantModel(0),seqs,True)
    assert lst1 == seqs
    assert lst2 == []
    
    lst1,lst2 = split(recessiveModel(3),seqs,True)
    assert lst1 == seqs[:2]
    assert lst2 == seqs[2:]


    try:
        split(DiseaseModel(0), seqs)
        assert False
    except NotImplementedError:
        pass

    af, unaf = split(DiseaseModel(0,predicate=lambda a: True), seqs, True)
    assert af == seqs
    assert unaf == []

    af, unaf = split(DiseaseModel([0,1],predicate=lambda a,b: True), seqs, True)
    assert af == seqs
    assert unaf == []

    def pred(a0,a2): return (a0,a2) == (1,1)
    af, unaf = split(DiseaseModel([0,2],predicate=pred), seqs)
    assert af == [[0,1]]
    assert unaf == [[0,1],[0,0],[0,0]]

    class DM(DiseaseModel):
        def __call__(self, a0, a2):
            return (a0,a2) == (1,1)
    af, unaf = split(DM([0,2]), seqs)
    assert af == [[0,1]]
    assert unaf == [[0,1],[0,0],[0,0]]


    def pred(p0,p2):
        a00, a02 = p0
        a20, a22 = p2
        return a00!=a20 and a02==a22
    dm = DiseaseModel([0,2],predicate=pred,
                      model=DiseaseModel.GENOTYPE_MODEL)
    af, unaf = split(dm, seqs)
    assert af == [[0,1],[0,1]]
    assert unaf == [[0,0],[0,0]]

    class DM(DiseaseModel):
        def __init__(self, indices):
            DiseaseModel.__init__(self,indices,
                                  model=DiseaseModel.GENOTYPE_MODEL)
        def __call__(self, p0, p2):
            a00, a02 = p0
            a20, a22 = p2
            return a00!=a20 and a02==a22

    af, unaf = split(DM([0,2]), seqs)
    assert af == [[0,1],[0,1]]
    assert unaf == [[0,0],[0,0]]
