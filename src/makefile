CXX = c++
CXXFLAGS = -Wall -O3 -lpopt

coasim_a_objects = simulator.o node.o interval.o retired_interval.o\
	 	 configuration.o builder.o descender.o \
		 snp_marker.o micro_satellite_marker.o trait_marker.o \
		 rc-parser.o

coasim_objects = coasim.o libcoasim.a

interval_test_objects = interval_test.o interval.o
all_markers_test_objects = all_markers_test.o snp_marker.o micro_satellite_marker.o trait_marker.o retired_interval.o interval.o node.o

rc-parser-test_objects = rc-parser-test.o rc-parser.o

mutation_test_objects = mutation_test.o snp_marker_t.o micro_satellite_marker_t.o trait_marker_t.o retired_interval.o interval.o node.o test_dist_funcs.o

configuration_test_objects = configuration.o configuration_test.o

node_test_objects = node_test.o node.o interval.o retired_interval.o configuration.o

builder_test_objects = builder_test.o node.o interval.o retired_interval.o \
			test_dist_funcs.o configuration.o builder.o

descender_test_objects = descender_test.o node.o interval.o retired_interval.o\
			configuration.o descender.o \
			snp_marker.o micro_satellite_marker.o trait_marker.o

simulator_test_objects = simulator_test.o simulator.o node.o interval.o retired_interval.o\
			configuration.o builder.o descender.o \
			snp_marker.o micro_satellite_marker.o trait_marker.o

profiler_objects = profiler.o simulator.o node.o interval.o retired_interval.o\
			configuration.o builder.o descender.o \
			snp_marker.o micro_satellite_marker.o trait_marker.o


test_programs = interval_test rc-parser-test all_markers_test configuration_test node_test builder_test descender_test mutation_test simulator_test

all: coasim test profiler

coasim : $(coasim_objects) 
	$(CXX) $(CXXFLAGS) -o coasim $(coasim_objects) 

libcoasim.a: $(coasim_a_objects)
	ar rcs libcoasim.a $(coasim_a_objects)

test: $(test_programs)
	if ! ./interval_test; then echo "FAILURE: interval_test failed!"; fi
	if ! ./configuration_test; then \
             echo "FAILURE: configuration_test failed!"; \
        fi
	if ! ./node_test; then echo "FAILURE: node_test failed!"; fi
	if ! ./all_markers_test; then \
	     echo "FAILURE: all_markers_test failed!"; \
        fi
	if ! ./mutation_test; then echo "FAILURE: mutation_test failed!"; fi
	if ! ./builder_test; then echo "FAILURE: builder_test failed!"; fi
	if ! ./descender_test; then echo "FAILURE: descender_test failed!"; fi
	if ! ./simulator_test; then echo "FAILURE: simulator_test failed!"; fi
	if ! ./rc-parser-test; then echo "FAILURE: rc-parser-test failed!"; fi

install: coasim
	cp coasim $(bindir)
	-mkdir $(sharedir)
	cp coasim.dtd $(sharedir)

profiler: $(profiler_objects)
	$(CXX) $(CXXFLAGS) -o profiler $(profiler_objects)

interval_test: $(interval_test_objects)
	$(CXX) $(CXXFLAGS) -o interval_test $(interval_test_objects)

rc-parser-test: $(rc-parser-test_objects)
	$(CXX) $(CXXFLAGS) -o rc-parser-test $(rc-parser-test_objects)

configuration_test: $(configuration_test_objects)
	$(CXX) $(CXXFLAGS) -o configuration_test $(configuration_test_objects)

node_test: $(node_test_objects)
	$(CXX) $(CXXFLAGS) -o node_test $(node_test_objects)

builder_test: $(builder_test_objects)
	$(CXX) $(CXXFLAGS) -o builder_test $(builder_test_objects)

all_markers_test: $(all_markers_test_objects)
	$(CXX) $(CXXFLAGS) -o all_markers_test $(all_markers_test_objects)

mutation_test: $(mutation_test_objects)
	$(CXX) $(CXXFLAGS) -o mutation_test $(mutation_test_objects)

descender_test: $(descender_test_objects)
	$(CXX) $(CXXFLAGS) -o descender_test $(descender_test_objects)

simulator_test: $(simulator_test_objects)
	$(CXX) $(CXXFLAGS) -o simulator_test $(simulator_test_objects)



coasim.o: coasim.cc node.o builder.o descender.hh reporter.hh dist_funcs.hh \
	  monitor.hh

interval.o: interval.hh interval.cc
interval_test.o: interval.hh interval.cc interval_test.cc testing.hh

retired_interval.o: retired_interval.hh retired_interval.cc

all_markers_test.o: marker.hh all_markers.hh snp_marker.hh trait_marker.hh micro_satellite_marker.hh all_markers_test.cc

mutation_test.o: marker.hh all_markers.hh snp_marker.hh trait_marker.hh micro_satellite_marker.hh all_markers_test.cc node.hh node.cc

configuration.o: configuration.hh configuration.cc
configuration_test.o: configuration_test.cc configuration.hh configuration.cc


node.o: node.hh node.cc dist_funcs.hh interval.hh marker.hh retired_interval.hh
	$(CXX) $(CXXFLAGS) -DDTD_DIR=\"$(sharedir)\" -c -o node.o node.cc
node_test.o: node_test.cc node.hh node.cc testing.hh interval.hh configuration.hh
	$(CXX) $(CXXFLAGS) -DTESTING -c -o node_test.o node_test.cc

builder.o: builder.hh builder.cc reporter.hh dist_funcs.hh node.hh monitor.hh
builder_test.o: builder.hh builder.cc builder_test.cc
	$(CXX) $(CXXFLAGS) -DTESTING -c -o builder_test.o builder_test.cc

descender.o: node.o interval.o dist_funcs.hh monitor.hh
descender_test.o: descender.hh descender_test.cc
	$(CXX) $(CXXFLAGS) -DTESTING -c -o descender_test.o descender_test.cc

snp_marker.o: snp_marker.hh snp_marker.cc marker.hh
trait_marker.o: trait_marker.hh trait_marker.cc marker.hh
micro_satellite_marker.o: micro_satellite_marker.hh micro_satellite_marker.cc marker.hh

snp_marker_t.o: snp_marker.hh snp_marker.cc marker.hh
	$(CXX) $(CXXFLAGS) -DTESTING -c -o snp_marker_t.o snp_marker.cc
trait_marker_t.o: trait_marker.hh trait_marker.cc marker.hh
	$(CXX) $(CXXFLAGS) -DTESTING -c -o trait_marker_t.o trait_marker.cc
micro_satellite_marker_t.o: micro_satellite_marker.hh micro_satellite_marker.cc marker.hh
	$(CXX) $(CXXFLAGS) -DTESTING -c -o micro_satellite_marker_t.o micro_satellite_marker.cc

simulator.o: simulator.hh simulator.cc builder.hh marker.hh descender.hh monitor.hh
simulator_test.o: simulator.hh simulator.cc simulator_test.cc
	$(CXX) $(CXXFLAGS) -DTESTING -c -o simulator_test.o simulator_test.cc

clean : 
	rm -f coasim *_test *.o

.PHONY: test test_programs clean
.SILENT: test
