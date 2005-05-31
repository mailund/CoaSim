/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "testing.hh"

#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef CORE__TRAIT_MARKER_HH_INCLUDED
# include "trait_marker.hh"
#endif
#ifndef CORE__SNP_MARKER_HH_INCLUDED
# include "snp_marker.hh"
#endif

using namespace core;


int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    std::vector<Marker*> markers;
    std::vector<Marker*>::iterator i;

    markers.push_back(new SNPMarker(0.0, 0.0,1.0));
    markers.push_back(new SNPMarker(0.2, 0.0,1.0));
    markers.push_back(new SNPMarker(0.4, 0.0,1.0));
    markers.push_back(new SNPMarker(0.3, 0.0,1.0));
    markers.push_back(new SNPMarker(0.67, 0.0,1.0));
    try {
	int pop_sizes[] = { 0 };
	double pop_fracs[] = { 1 };
	Epoch *dummy_epoch_itr = 0;
	Configuration conf(pop_sizes, pop_sizes+1,
			   pop_fracs, pop_fracs+1,
			   markers.begin(), markers.end(),
			   &dummy_epoch_itr, &dummy_epoch_itr,
			   0.0, 0.0, 0.0, 0.0);
	ERROR("pop size <= 0");
    } catch (Configuration::non_pos_pop_size&) {}

    try {
	int pop_sizes[] = { -1 };
	double pop_fracs[] = { 1 };
	Epoch *dummy_epoch_itr = 0;
	Configuration conf(pop_sizes, pop_sizes+1,
			   pop_fracs, pop_fracs+1,
			   markers.begin(), markers.end(),
			   &dummy_epoch_itr, &dummy_epoch_itr,
			   0.0, 0.0, 0.0, 0.0);
	ERROR("pop size <= 0");
    } catch (Configuration::non_pos_pop_size&) {}

    try {
	int pop_sizes[] = { 1 };
	double pop_fracs[] = { 1, 1 };
	Epoch *dummy_epoch_itr = 0;
	Configuration conf(pop_sizes, pop_sizes+1,
			   pop_fracs, pop_fracs+2,
			   markers.begin(), markers.end(),
			   &dummy_epoch_itr, &dummy_epoch_itr,
			   0.0, 0.0, 0.0, 0.0);
	ERROR("inconsistent pop and frac");
    } catch (Configuration::inconsistent_pop_spec&) {}

    try {
	int pop_sizes[] = { 1 };
	double pop_fracs[] = { 1 };
	Epoch *dummy_epoch_itr = 0;
	Configuration conf(pop_sizes, pop_sizes+1,
			   pop_fracs, pop_fracs+1,
			   markers.begin(), markers.end(),
			   &dummy_epoch_itr, &dummy_epoch_itr,
			   0.0, 0.0, 0.0, 0.0);
	ERROR("Unsorted positions");
    } catch (Configuration::out_of_sequence&) {}


    for (i = markers.begin(); i != markers.end(); ++i)
	delete *i;
    markers.resize(0);


    const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
    const size_t no_positions = (sizeof positions)/sizeof(double);
    markers.push_back(new SNPMarker(0.0, 0.0,1.0));
    markers.push_back(new SNPMarker(0.2, 0.0,1.0));
    markers.push_back(new SNPMarker(0.3, 0.0,1.0));
    markers.push_back(new SNPMarker(0.4, 0.0,1.0));
    markers.push_back(new SNPMarker(0.67, 0.0,1.0));

    try {

	int pop_sizes[] = { 1 };
	double pop_fracs[] = { 1 };
	Epoch *dummy_epoch_itr = 0;
	Configuration conf(pop_sizes, pop_sizes+1,
			   pop_fracs, pop_fracs+1,
			   markers.begin(), markers.end(),
			   &dummy_epoch_itr, &dummy_epoch_itr,
			   0.0, 0.0, 0.0, 0.0);
	for (size_t i = 0; i < no_positions; ++i)
	    CHECK(conf.position(i) == positions[i]);


  
    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    
    for (i = markers.begin(); i != markers.end(); ++i)
	delete *i;
    markers.resize(0);


    REPORT_RESULTS;
}
