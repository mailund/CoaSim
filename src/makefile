CXX = c++
CXXFLAGS = -Wall -O3

objects = coasim.o node.o builder.o interval.o 
source = coasim.cc node.cc node.hh builder.cc builder.hh interval.cc interval.hh reporter.hh dist_funcs.hh descender.hh

coasim : $(objects)
	$(CXX) $(CXXFLAGS) -o coasim $(objects)

coasim.o : coasim.cc node.o builder.o descender.hh reporter.hh dist_funcs.hh
node.o : node.hh node.cc dist_funcs.hh interval.o
builder.o : builder.hh builder.cc reporter.hh dist_funcs.hh node.o
interval.o : interval.hh interval.cc

clean : 
	rm -f coasim $(objects)
