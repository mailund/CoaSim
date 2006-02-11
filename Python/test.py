import CoaSim

markers = [CoaSim.SNPMarker(0.2, 0.1, 0.9),
           CoaSim.SNPMarker(0.3, 0.1, 0.9),
           CoaSim.TraitMarker(0.5, 0.2, 0.4),
           CoaSim.SNPMarker(0.6, 0.1, 0.9),
           CoaSim.MicroSatelliteMarker(0.7, 1e-3, 10),
           CoaSim.SNPMarker(0.8, 0.1, 0.9)]

arg = CoaSim.simulate(markers, 10, rho=40, seed=10)
print arg.sequences
