/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */


#include "testing.hh"
#ifndef ALL_MARKERS_HH_INCLUDED
# include "all_markers.hh"
#endif

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

	CHECK(snp_marker.size() == 2);
	CHECK(snp_marker.value(0) == 0);
	CHECK(snp_marker.value(1) == 1);

	try {
	    snp_marker.value(2);
	    ERROR("Cannot access index 2");
	} catch(std::out_of_range&) {}


	TraitMarker trait_marker(0.0,0.0,1.0);

	try {
	    trait_marker.add_value(-1);
	    ERROR("We cannot set a Trait value -- and not to -1");
	} catch(Marker::illegal_value&) {}

	try {
	    trait_marker.add_value(0);
	    ERROR("We cannot set a Trait value");
	} catch(Marker::illegal_value&) {}

	try {
	    trait_marker.add_value(1);
	    ERROR("We cannot set a Trait value");
	} catch(Marker::illegal_value&) {}

	try {
	    trait_marker.add_value(2);
	    ERROR("We cannot set a Trait value");
	} catch(Marker::illegal_value&) {}

	CHECK(trait_marker.size() == 2);
	CHECK(trait_marker.value(0) == 0);
	CHECK(trait_marker.value(1) == 1);

	try {
	    trait_marker.value(2);
	    ERROR("Cannot access index 2");
	} catch(std::out_of_range&) {}



	MicroSatelliteMarker ms_marker(0.0,0.0);

	CHECK(ms_marker.size() == 0);

	try {
	    ms_marker.add_value(-1);
	    ERROR("We cannot set a micro satellite value to a negative value.");
	} catch(Marker::illegal_value&) {}
	CHECK(ms_marker.size() == 0);

	ms_marker.add_value(0);
	ms_marker.add_value(2);
	ms_marker.add_value(3);
	CHECK(ms_marker.size() == 3);
	CHECK(ms_marker.value(0) == 0);
	CHECK(ms_marker.value(1) == 2);
	CHECK(ms_marker.value(2) == 3);

	try {
	    ms_marker.value(3);
	    ERROR("Accessing out of range.");
	} catch(std::out_of_range&) {}


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
    
