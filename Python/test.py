from exceptions import Exception
from CoaSim import *

class reject(Exception): pass
class RejectionSampler(object):
    def recombinationEvent(self,n1,n2,k):
        raise reject()
rejectionSampler = RejectionSampler()

noSamples = 0
heights = []

while noSamples < 1000:
    try:
        arg = simulate([], 5, rho=2, keepEmptyIntervals=True,
                       callbacks=rejectionSampler)
        tree = arg.intervals[0].tree
        heights.append(tree.height)
        noSamples += 1
    except reject:
        pass

print 'Average tree height:', sum(heights)/len(heights)
