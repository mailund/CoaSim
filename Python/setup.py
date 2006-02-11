#!/bin/env python

from distutils.core import setup, Extension

DESCRIPTION = """
CoaSim is a tool for simulating the coalescent process with recombination
and geneconversion under various demographic models. It effectively
constructs the ancestral recombination graph for a given number of
individuals and uses this to simulate samples of SNP, micro-satellite,
and other haplotypes/genotypes. The generated sample can afterwards be
separated in cases and controls, depending on states of selected individual
markers. The tool can accordingly also be used to construct cases and
control data sets for association studies.
"""

CoaSim_Core = Extension('CoaSim.Core', ['markers.cc', 'arg.cc',
                                        'simulate.cc', 'main.cc'],
                        include_dirs=['..'],
                        library_dirs=['../Core'],
                        libraries=['coasim', 'stdc++'])

setup(name="coasim-python",
      version="0.2",

      author="Thomas Mailund",
      author_email="mailund@birc.au.dk",
      url='http://www.daimi.au.dk/~mailund/CoaSim/',
      license='GNU General Public License (GPL)',

      description='Coalescent-based population genetics simulation program',
      long_description=DESCRIPTION,

      scripts=[],
      
      package_dir={'':'modules'},
      packages=['CoaSim'],
      py_modules=['CoaSim.randomMarkers'],
      ext_modules=[CoaSim_Core],
      )
