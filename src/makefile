CXX = c++
CXXFLAGS = -Wall -O3

objects = coasim.o node.o builder.o interval.o 
source = coasim.cc \
         interval.cc interval.hh \
         node.cc node.hh node_test.cc \
         builder.cc builder.hh \
         reporter.hh dist_funcs.hh descender.hh

interval_test_objects = interval_test.o interval.o
node_test_objects = node_test.o node_t.o interval.o test_dist_funcs.o

coasim : $(objects)
	$(CXX) $(CXXFLAGS) -o coasim $(objects)

test: test_programs
	if ! ./interval_test; then echo "FAILURE: interval_test failed!"; fi
	if ! ./node_test;     then echo "FAILURE: node_test failed!"; fi

test_programs: interval_test node_test

interval_test: $(interval_test_objects)
	$(CXX) $(CXXFLAGS) -o interval_test $(interval_test_objects)

node_test: $(node_test_objects)
	$(CXX) $(CXXFLAGS) -o node_test $(node_test_objects)



coasim.o: coasim.cc node.o builder.o descender.hh reporter.hh dist_funcs.hh

interval.o: interval.hh interval.cc
interval_test.o: interval.hh interval.cc interval_test.cc testing.hh

node.o: node.hh node.cc dist_funcs.hh

node_t.o: node.hh node.cc dist_funcs.hh
	$(CXX) $(CXXFLAGS) -DTESTING -c -o node_t.o node.cc

node_test.o: node_test.cc node.hh node.cc testing.hh
	$(CXX) $(CXXFLAGS) -DTESTING -c -o node_test.o node_test.cc

builder.o: builder.hh builder.cc reporter.hh dist_funcs.hh node.o



clean : 
	rm -f coasim *_test *.o

.PHONY: test test_programs clean
.SILENT: test
