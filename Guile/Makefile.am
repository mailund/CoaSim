
INCLUDES = -I..
LIBS = -L../Core -lcoasim -lguile -lpopt
LDFLAGS = -Wl,-rpath,/usr/local/lib

noinst_PROGRAMS =
noinst_PROGRAMS += marker_test
noinst_PROGRAMS += simulate_test

bin_PROGRAMS =
bin_PROGRAMS += coasim_guile


TESTS =
TESTS += marker_test.sh
TESTS += simulate_test.sh

EXTRA_DIST =
EXTRA_DIST += marker_test.sh marker-test.scm marker-test.expected
EXTRA_DIST += simulate_test.sh simulate-test.scm simulate-test.expected

marker_test_SOURCES =
marker_test_SOURCES += marker.hh marker.cc 
marker_test_SOURCES += marker_test.cc

simulate_test_SOURCES =
simulate_test_SOURCES += marker.hh marker.cc 
simulate_test_SOURCES += options.hh options.cc
simulate_test_SOURCES += nodes.hh nodes.cc
simulate_test_SOURCES += intervals.hh intervals.cc
simulate_test_SOURCES += simulate.hh simulate.cc
simulate_test_SOURCES += simulate_test.cc

coasim_guile_SOURCES =
coasim_guile_SOURCES += marker.hh marker.cc 
coasim_guile_SOURCES += options.hh options.cc
coasim_guile_SOURCES += nodes.hh nodes.cc
coasim_guile_SOURCES += intervals.hh intervals.cc
coasim_guile_SOURCES += simulate.hh simulate.cc
coasim_guile_SOURCES += coasim_guile.cc


docdir = ${prefix}/share/coasim/doc
dist_doc_DATA =
dist_doc_DATA += doc/bindings.html doc/guile-bindings.css
doc/index.html:
	(cd doc; make)

EXTRA_DIST += doc/extract_guile_comments.py \
              doc/guile-bindings.xsl \
              doc/htmltags.xsl \
              doc/Makefile


test_inputdir = ${prefix}/share/coasim/test-input
dist_test_input_DATA =

dist_test_input_DATA += test-input/tree.scm
dist_test_input_DATA += test-input/more-trees.scm
dist_test_input_DATA += test-input/mean-branch-length.scm
dist_test_input_DATA += test-input/more-mean-branch-length.scm
dist_test_input_DATA += test-input/fold-mean-branch-length.scm
dist_test_input_DATA += test-input/sequences.scm
dist_test_input_DATA += test-input/more-sequences.scm
dist_test_input_DATA += test-input/snp-haplotype.scm
dist_test_input_DATA += test-input/snp-haplotype-split.scm
dist_test_input_DATA += test-input/ms.scm


schemedir = ${prefix}/share/coasim/scheme
dist_scheme_DATA =
scheme_coasimdir = ${prefix}/share/coasim/scheme/coasim
dist_scheme_coasim_DATA =
dist_scheme_coasim_DATA += scheme/coasim/batch.scm
dist_scheme_coasim_DATA += scheme/coasim/io.scm
dist_scheme_coasim_DATA += scheme/coasim/rand.scm
dist_scheme_coasim_DATA += scheme/coasim/markers.scm

scheme_coasim_SNPdir = ${prefix}/share/coasim/scheme/coasim/SNP
dist_scheme_coasim_SNP_DATA =
dist_scheme_coasim_SNP_DATA += scheme/coasim/SNP/haplotypes.scm
dist_scheme_coasim_SNP_DATA += scheme/coasim/SNP/genotypes.scm
