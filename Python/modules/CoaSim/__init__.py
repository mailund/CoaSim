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
    Coalescent Models
    T. Mailund, M.H. Schierup, C.N.S. Pedersen, P.J.M. Mechlenborg,
    J.N. Madsen, and L. Schauser
    BMC Bioinformatics 2005, 6:252. doi:10.1186/1471-2105-6-252.

CoaSim is developed in collaboration between Bioinformatics ApS and
Bioinformatics Research Center (BiRC) and released under the GNU
General Public License.

Please see http://www.daimi.au.dk/~mailund/CoaSim/index.html for more
details.'''

from CoaSim.Core import *
