
#ifndef CORE__BUILDER_HH_INCLUDED
# include "builder.hh"
#endif
#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef CORE__SNP_MARKER_HH_INCLUDED
# include "snp_marker.hh"
#endif

#include "testing.hh"

using namespace core;

/*
 * WARNING: THIS PROGRAM IS NOT REALLY TESTING BUILDER -- WE NEED TO
 * DO THAT STATISTICALLY SOMEHOW -- THE PROGRAM SIMPLY TRIES TO RUN
 * THE BUILDER (TO SEE THAT IT DOESN'T CRASH).  MOST OF THE REALY
 * FUNCTIONALITY IS IMPLEMENTED AND TESTED IN THE NODE MODULE ANYWAY.
 */


int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {

	std::vector<Marker*> markers;
	markers.push_back(new SNPMarker(0.0, 0.0,1.0));
	markers.push_back(new SNPMarker(0.2, 0.0,1.0));
	markers.push_back(new SNPMarker(0.3, 0.0,1.0));
	markers.push_back(new SNPMarker(0.4, 0.0,1.0));
	markers.push_back(new SNPMarker(0.67, 0.0,1.0));
   

	Configuration conf(10,
			   markers.begin(), markers.end(),
			   0.0, 0.0, 0.0, 0.0);

	std::vector<Marker*>::iterator i;
	for (i = markers.begin(); i != markers.end(); ++i)
	    delete *i;
	markers.resize(0);


	Builder b(conf);
	ARG *arg = b.build(0);

	CHECK(arg != 0);

    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    REPORT_RESULTS;
}
