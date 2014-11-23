#!/bin/env python

from distutils.core import setup

DESCRIPTION = """
CoaSim/Python -- Python bindings for Coasim
Copyright (C) 2006, 2014 by Thomas Mailund <mailund@birc.au.dk>

CoaSim is a tool for simulating the coalescent process with recombination
and geneconversion under various demographic models. It effectively
constructs the ancestral recombination graph for a given number of
individuals and uses this to simulate samples of SNP, micro-satellite,
and other haplotypes/genotypes. The generated sample can afterwards be
separated in cases and controls, depending on states of selected individual
markers. The tool can accordingly also be used to construct cases and
control data sets for association studies.

To cite CoaSim, please use:

    CoaSim: A Flexible Environment for Simulating Genetic Data under
    Coalescent Models
    T. Mailund, M.H. Schierup, C.N.S. Pedersen, P.J.M. Mechlenborg,
    J.N. Madsen, and L. Schauser
    BMC Bioinformatics 2005, 6:252. doi:10.1186/1471-2105-6-252.

CoaSim is developed in collaboration between Bioinformatics ApS and
Bioinformatics Research Center (BiRC) and released under the GNU
General Public License.

Please see http://users-birc.au.dk/mailund/CoaSim/ for more
details."""


setup(name="CoaSim",
      version="2.0",

      author="Thomas Mailund",
      author_email="mailund@birc.au.dk",
      url='http://users-birc.au.dk/mailund/CoaSim/',
      license='GNU General Public License (GPL)',

      description='Coalescent-based population genetics simulation program',
      long_description=DESCRIPTION,

      scripts=[],
      
      package_dir={'':'/Users/mailund/Projects/CoaSim/Python/modules'},
      packages=['CoaSim'],
      py_modules=['CoaSim.randomMarkers'],
      package_data={"": ['CoaSimCore.so']},
      )
