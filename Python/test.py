from CoaSim import *


from random import uniform, expovariate 
class StepWise(CustomMarker):
    '''The step-wise mutation model for micro-satellites.'''

    def __init__(self, pos, theta):
        '''Initialize marker at position pos with mutation rate theta
        -- the intensity of the exponetial waiting time for the next
        mutation event will be theta/2.'''
        CustomMarker.__init__(self,pos) # don't forget!
        self.lambd = float(theta)/2

    def _mutateTo(self, allele):
        '''Choose a new allele -- randomly one step up or one step
        down.'''
        if uniform(0.0,1.0) < 0.5: return allele-1
        else:                      return allele+1

    def _wait(self):
        '''Draw the waiting time for the next mutation.'''
        return expovariate(self.lambd)

    def defaultValue(self):
        '''Initial value -- here always 0.'''
        return 0

    def mutate(self, parentAllele, edgeLength):
        '''Step-wise mutations...'''
        time = self._wait()
        allele = parentAllele
        while time < edgeLength: # mutate till end of the edge
            allele = self._mutateTo(allele)
            time += self._wait()
        return allele


markers = [StepWise(0,3), StepWise(0.5,3), StepWise(0.9999999,300)]

print simulate(markers, 5).sequences
