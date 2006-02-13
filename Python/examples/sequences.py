from CoaSim import simulate
from CoaSim.randomMarkers import makeRandomSNPMarkers

markers = makeRandomSNPMarkers(10, 0.0, 1.0)
sequences = simulate(markers, 100, rho=400).sequences

from CoaSim.IO import printMarkerPositions, printSequences
printMarkerPositions(markers, open('positions.txt','w'))
printSequences(sequences, open('sequences.txt','w'))

