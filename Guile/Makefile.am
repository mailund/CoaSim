
INCLUDES = -I..
LIBS = -L../Core -lcoasim -lguile -lpopt

noinst_PROGRAMS =
noinst_PROGRAMS += marker_test
noinst_PROGRAMS += arg_parameters_test
noinst_PROGRAMS += simulate_test

bin_PROGRAMS =
bin_PROGRAMS += coasim_guile


TESTS =
TESTS += marker_test.sh
TESTS += arg_parameters_test.sh
TESTS += simulate_test.sh

EXTRA_DIST =
EXTRA_DIST += marker_test.sh marker-test.scm marker-test.expected
EXTRA_DIST += arg_parameters_test.sh arg-parameters-test.scm \
              arg-parameters-test.expected
EXTRA_DIST += simulate_test.sh simulate-test.scm simulate-test.expected

marker_test_SOURCES =
marker_test_SOURCES += marker.hh marker.cc 
marker_test_SOURCES += marker_test.cc

arg_parameters_test_SOURCES =
arg_parameters_test_SOURCES += arg_parameters.hh arg_parameters.cc 
arg_parameters_test_SOURCES += arg_parameters_test.cc

simulate_test_SOURCES =
simulate_test_SOURCES += marker.hh marker.cc 
simulate_test_SOURCES += arg_parameters.hh arg_parameters.cc 
simulate_test_SOURCES += options.hh options.cc
simulate_test_SOURCES += simulate.hh simulate.cc
simulate_test_SOURCES += simulate_test.cc

coasim_guile_SOURCES =
coasim_guile_SOURCES += marker.hh marker.cc 
coasim_guile_SOURCES += arg_parameters.hh arg_parameters.cc 
coasim_guile_SOURCES += options.hh options.cc
coasim_guile_SOURCES += simulate.hh simulate.cc
coasim_guile_SOURCES += coasim_guile.cc
