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
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include "all_markers.hh"
#endif
#ifndef CORE__TEST_DIST_FUNCS_HH_INCLUDED
# include "test_dist_funcs.hh"
#endif

using namespace core;

int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {


	std::vector<Marker*> markers;
	markers.push_back(new SNPMarker(0.0, 0.0,0.4));
	markers.push_back(new SNPMarker(0.2, 0.0,1.0));
	markers.push_back(new TraitMarker(0.3, 0.0,1.0));
	markers.push_back(new TraitMarker(0.31, 0.0,0.4));
	markers.push_back(new TraitMarker(0.32, 0.0,0.5));
	MicroSatelliteMarker *ms_m = new MicroSatelliteMarker(0.4, 0.7, 2);
	markers.push_back(ms_m);
	markers.push_back(new SNPMarker(0.67, 0.0,1.0));
	markers.push_back(new SNPMarker(0.68, 0.0,0.4));
	markers.push_back(new SNPMarker(0.69, 0.0,0.5));
   

	unsigned int pop_sizes[] = { 2 };
	Epoch *dummy_epoch_itr = 0;
	Configuration conf(pop_sizes, pop_sizes+1,
			   markers.begin(), markers.end(),
			   &dummy_epoch_itr, &dummy_epoch_itr,
			   0.0, 0.0, 0.0, 0.0);

	std::vector<Marker*>::iterator i;
	for (i = markers.begin(); i != markers.end(); ++i)
	    delete *i;
	markers.resize(0);


	ARG arg(conf);

	Node *l1 = arg.leaf();
	Node *l2 = arg.leaf();
	ARG::recomb_node_pair_t rp;
	rp = arg.recombination(1.0,l1,0.5);
	Node *r1 = rp.first;
	Node *r2 = rp.second;
	ARG::gene_conv_node_pair_t gp = arg.gene_conversion(2.0, l2, 0.30, 0.60);
	Node *g1  = gp.first;
	Node *g2  = gp.second;
	Node *c1  = arg.coalescence(3.0, r2, g1);
	Node *c2  = arg.coalescence(4.0, r1, g2);
	Node *top = arg.coalescence(5.0, c1, c2);


	// ARG retired intervals:
	// 0: [0.6--1.0)  s =  6.0   markers: 6 7 8
	// 1: [0.3--0.5)  s =  8.0   markers: 2 3 4 5
	// 2: [0.0--0.3)  s = 10.0   markers: 0 1
	// 3: [0.5--0.6)  s = 10.0   markers:

	// Tree for interval 0:
	// 
	//       markers: 6 7 8
	// 
	//          c1
	//         /  \ 1.0
	//    2.0 /    g1
	//       /      |
	//      r2      | 2.0
	//  1.0 |       |
	//      l1      l2


	// SNP mutation, put mutation on c1->r2 edge
	Distribution_functions::uniform_result = 0.0;
	arg.retired_intervals().at(0).mutate(conf,6);

	CHECK(c1->state(6) == 0);
	CHECK(r2->state(6) == 1);
	CHECK(l1->state(6) == 1);
	CHECK(g1->state(6) == 0);
	CHECK(l2->state(6) == 0);

	// try with frequency restrictions leading to a retry...
	try {
	    arg.retired_intervals().at(0).mutate(conf,7);
	    ERROR("This should have lead to a retry...");
	} catch (Mutator::retry_mutation&) {}

	// this should be accepted
	arg.retired_intervals().at(0).mutate(conf,8);


	CHECK(c1->state(8) == 0);
	CHECK(r2->state(8) == 1);
	CHECK(l1->state(8) == 1);
	CHECK(g1->state(8) == 0);
	CHECK(l2->state(8) == 0);

    

	// Tree for interval 1:
	// 
	//       markers: 2 3 4 5
	// 
	//          c2
	//         /  \ 2.0
	//        /    |
	//   3.0 /     g2
	//       |      |
	//       r1     | 2.0
	//  1.0  |      |
	//       l1     l2

	// Trait mutation, put mutation on c2->g2 edge
	Distribution_functions::uniform_result = 0.5;
	arg.retired_intervals().at(1).mutate(conf,2);

	CHECK(c2->state(2) == 0);
	CHECK(r1->state(2) == 0);
	CHECK(l1->state(2) == 0);
	CHECK(g2->state(2) == 1);
	CHECK(l2->state(2) == 1);


	// try with frequency restrictions leading to a retry...
	try {
	    arg.retired_intervals().at(1).mutate(conf,3);
	    ERROR("This should have lead to a retry...");
	} catch (Mutator::retry_arg&) {}

	// this should be accepted
	arg.retired_intervals().at(1).mutate(conf,4);

	CHECK(c2->state(4) == 0);
	CHECK(r1->state(4) == 0);
	CHECK(l1->state(4) == 0);
	CHECK(g2->state(4) == 1);
	CHECK(l2->state(4) == 1);




	// micro-satellite mutation, put mutation on all edges 
	Distribution_functions::uniform_result = 0.5;
	Distribution_functions::expdist_result = 0.6;
	arg.retired_intervals().at(1).mutate(conf,5);

	CHECK(c2->state(5) == 42);
	CHECK(r1->state(5) == 86);
	CHECK(l1->state(5) == 86);
	CHECK(g2->state(5) == 86);
	CHECK(l2->state(5) == 86);

	// Tree for interval 2:
	// 
	//       markers: 0 1
	// 
	//          top
	//         /  \ 1.0
	//    2.0 /    c2
	//       /      |
	//      c1      |
	//  1.0 |       |
	//      g1      | 3.0
	//      |       |
	//  2.0 |       |
	//      |       |
	//      r2      r1
	//  1.0 |       | 1.0
	//      l2      l1

	// Tree for interval 3:
	// 
	//       markers:
	// 
	//          top
	//         /  \ 1.0
	//    2.0 /    c2
	//       /      |
	//      c1      | 2.0
	//      |       |
	//  2.0 |      g2
	//      |       |
	//      r2      | 2.0
	//  1.0 |       |
	//      l2      l2
 
   
	// to avoid warnings
	top = 0;


    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    REPORT_RESULTS;
}
