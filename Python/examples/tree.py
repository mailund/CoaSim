from CoaSim import simulate, SNPMarker

arg = simulate([SNPMarker(0.5,0,1)], 10)
tree = arg.intervals[0].tree
print tree
