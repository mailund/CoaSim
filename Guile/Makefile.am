
INCLUDES = -I..
LIBS = -L../Core -lcoasim -lguile

noinst_PROGRAMS =
noinst_PROGRAMS += marker_test
noinst_PROGRAMS += arg_parameters_test

TESTS =
TESTS += marker_test.sh
TESTS += arg_parameters_test.sh

EXTRA_DIST =
EXTRA_DIST += marker_test.sh marker-test.scm marker-test.expected
EXTRA_DIST += arg_parameters_test.sh arg-parameters-test.scm \
              arg-parameters-test.expected

marker_test_SOURCES =
marker_test_SOURCES += marker.hh marker.cc 
marker_test_SOURCES += marker_test.cc

arg_parameters_test_SOURCES =
arg_parameters_test_SOURCES += arg_parameters.hh arg_parameters.cc 
arg_parameters_test_SOURCES += arg_parameters_test.cc

