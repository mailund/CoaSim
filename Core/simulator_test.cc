
#include "testing.hh"

#include "simulator.hh"

#include "configuration.hh"
#include "all_markers.hh"

/*
 * WARNING: THIS PROGRAM IS NOT REALLY TESTING SIMULATOR -- WE NEED TO
 * DO THAT STATISTICALLY SOMEHOW -- THE PROGRAM SIMPLY TRIES TO RUN
 * THE SIMULATOR (TO SEE THAT IT DOESN'T CRASH).
 */


int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {

	const unsigned int no_leaves = 2;
	const double rho = 1.0;	// 5.0
	const double Q = 0.0;	// 250.0
	const double G = 0.0;	// 250.0
	const double growth = 5.0;
	const double mu = 5.0;
	std::vector<Marker*> markers;
	markers.push_back(new SNPMarker(0.0, 0.0,1.0));
	markers.push_back(new TraitMarker(0.2, 0.0,1.0));
	markers.push_back(new SNPMarker(0.3, 0.0,1.0));
	MicroSatelliteMarker *mm = new MicroSatelliteMarker(0.4, mu);
	mm->add_value(4); mm->add_value(8);
	markers.push_back(mm);
	markers.push_back(new SNPMarker(0.67, 0.0,1.0));


	Configuration conf(no_leaves,
			   markers.begin(), markers.end(),
			   rho, Q, G, growth);

	// FIXME: not completely exception safe -- memory leak if
	// exception thrown... who cares, it's only a test after all...
	std::vector<Marker*>::iterator i;
	for (i = markers.begin(); i != markers.end(); ++i)
	    delete *i;
	markers.resize(0);
	ARG *arg = Simulator::simulate(conf);
	CHECK(arg != 0);


    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    REPORT_RESULTS;
}
