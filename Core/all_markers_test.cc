/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */


#include "testing.hh"
#include "all_markers.hh"

using namespace core;

int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {

	SNPMarker snp_marker(0.0,0.0,1.0);

	try {
	    snp_marker.add_value(-1);
	    ERROR("We cannot set a SNP value -- and not to -1");
	} catch(Marker::illegal_value&) {}

	try {
	    snp_marker.add_value(0);
	    ERROR("We cannot set a SNP value");
	} catch(Marker::illegal_value&) {}

	try {
	    snp_marker.add_value(1);
	    ERROR("We cannot set a SNP value");
	} catch(Marker::illegal_value&) {}

	try {
	    snp_marker.add_value(2);
	    ERROR("We cannot set a SNP value");
	} catch(Marker::illegal_value&) {}


	TraitMarker trait_marker(0.0,0.0,1.0);


	MicroSatelliteMarker ms_marker(0.0,1.0,3);

	CHECK(ms_marker.K() == 3);

	SNPMarker sm(0.2, 0, 1);
	CHECK(sm.position() == 0.2);

	Marker *m = &sm;
	CHECK(m->position() == 0.2);
	Marker *copy = m->copy();
	CHECK(copy->position() == 0.2);
	


    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }


    REPORT_RESULTS;
}
    
