CXX = c++
CXXFLAGS = -Wall -O3

objects = coasim.o node.o builder.o interval.o 
source = coasim.cc \
         node.cc node.hh node_test.cc \
         builder.cc builder.hh \
         interval.cc interval.hh \
         reporter.hh dist_funcs.hh descender.hh

coasim : $(objects)
	$(CXX) $(CXXFLAGS) -o coasim $(objects)

test: test_programs
	if ! ./interval_test; then echo "FAILURE: interval_test failed!"; fi
	if ! ./node_test;     then echo "FAILURE: node_test failed!"; fi

test_programs: interval_test node_test

interval_test: interval_test.o interval.o
	$(CXX) $(CXXFLAGS) -o interval_test interval_test.o interval.o

node_test: node_test.o node.o interval.o
	$(CXX) $(CXXFLAGS) -o node_test node_test.o node.o  interval.o



coasim.o : coasim.cc node.o builder.o descender.hh reporter.hh dist_funcs.hh

interval.o : interval.hh interval.cc
interval_test.o: interval.hh interval.cc interval_test.cc testing.hh

node.o : node.hh node.cc dist_funcs.hh interval.o
node_test.o: node.hh node.cc

builder.o : builder.hh builder.cc reporter.hh dist_funcs.hh node.o



clean : 
	rm -f coasim $(objects)

.PHONY: test test_programs clean
.SILENT: test
