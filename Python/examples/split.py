from CoaSim import simulate, TraitMarker, insertSorted
from CoaSim.randomMarkers import randomPosition, makeRandomSNPMarkers
from CoaSim.diseaseModelling import singleMarkerDisease, split
from CoaSim.IO import printSequences

markers = makeRandomSNPMarkers(10, 0.0, 1.0)
traitMarker = TraitMarker(randomPosition(), 0.2, 0.4)
markers,traitIdx = insertSorted(markers, traitMarker)

sequences = simulate(markers, 100, rho=400).sequences
diseaseModel = \
   singleMarkerDisease(traitIdx,wildTypeRisk=0.1,mutantRisk=0.3)
affected, unaffected = split(diseaseModel,sequences)

print 'Affected:'
printSequences(affected)
print
print 'Unaffected:'
printSequences(unaffected)
print

