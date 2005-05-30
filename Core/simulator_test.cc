/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "testing.hh"

#include "epochs.hh"
#include "simulator.hh"

#include "configuration.hh"
#include "all_markers.hh"

using namespace core;


/*
 * WARNING: THIS PROGRAM IS NOT REALLY TESTING THE SIMULATOR -- WE
 * NEED TO DO THAT STATISTICALLY SOMEHOW -- THE PROGRAM SIMPLY TRIES
 * TO RUN THE SIMULATOR (TO SEE THAT IT DOESN'T CRASH).
 */


int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {

	const unsigned int no_leaves = 10;
	const double rho = 1.0;
	const double Q = 3.0;
	const double G = 2.0;
	const double growth = 0.1; //5.0;
	const double mu = 5.0;
	std::vector<Marker*> markers;
	markers.push_back(new SNPMarker(0.0, 0.0,1.0));
	markers.push_back(new TraitMarker(0.2, 0.0,1.0));
	markers.push_back(new SNPMarker(0.3, 0.0,1.0));
	MicroSatelliteMarker *mm = new MicroSatelliteMarker(0.4, mu, 2);
	markers.push_back(mm);
	markers.push_back(new SNPMarker(0.67, 0.0,1.0));

	std::vector<Epoch*> epochs;
	epochs.push_back(new BottleNeck(0.1, 1.0, 2.0));

	Configuration conf(no_leaves,
			   markers.begin(), markers.end(),
			   epochs.begin(),  epochs.end(),
			   rho, Q, G, growth);

	// FIXME: not completely exception safe -- memory leak if
	// exception thrown... who cares, it's only a test after all...
	std::vector<Marker*>::iterator i;
	for (i = markers.begin(); i != markers.end(); ++i)
	    delete *i;
	markers.resize(0);
	std::vector<Epoch*>::iterator j;
	for (j = epochs.begin(); j != epochs.end(); ++j)
	    delete *j;
	epochs.resize(0);

	ARG *arg = Simulator::simulate(conf);
	CHECK(arg != 0);


    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    REPORT_RESULTS;
}
