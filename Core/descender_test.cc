/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef CORE__DESCENDER_HH_INCLUDED
# include "descender.hh"
#endif
#ifndef CORE__CONFIGUTAION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include "all_markers.hh"
#endif

#include "testing.hh"

using namespace core;

/*
 * WARNING: THIS PROGRAM IS NOT REALLY TESTING DESCENDER -- WE NEED TO
 * DO THAT STATISTICALLY SOMEHOW -- THE PROGRAM SIMPLY TRIES TO RUN
 * THE DESCENDER (TO SEE THAT IT DOESN'T CRASH).  MOST OF THE REALY
 * FUNCTIONALITY IS IMPLEMENTED AND TESTED IN THE NODE MODULE ANYWAY.
 */


static ARG *build_arg(Configuration &conf)
{
    ARG *arg = new ARG(conf);

    Node *l1 = arg->leaf();
    Node *l2 = arg->leaf();

    // current ARG:
    //
    //   (l1: [0---1) )    (l2: [0---1) )

    ARG::node_pair_t p;

    p = arg->recombination(0.0,l1,0.5);
    Node *r1 = p.first;
    Node *r2 = p.second;

    // current ARG:
    //
    //  (r1: [0--0.5) )  (r2: [0.5--1) )
    //         \              /
    //          \            /
    //          (l1: [0---1) )                  (l2: [0---1) )


    p = arg->gene_conversion(0.0, l2, 0.30, 0.60);
    Node *g1 = p.first;
    Node *g2 = p.second;

    // current ARG:
    //
    //  (r1: [0--0.5) ) (r2: [0.5--1) ) (g1: [0-0.3)[0.6-1.0) ) (r2: [0.3--0.6) )
    //         \             /                     \              /
    //          \           /                       \            /
    //          (l1: [0---1) )                      (l2: [0---1) )


    Node *c1 = arg->coalescence(0.0, r2, g1);

    // current ARG:
    // 
    //                   (c1: [0-0.3)[0.5-0.6)[0.6-1.0) )
    //                              /    \    `-------' <- retired
    //                             /      \                         --no nl esc
    //  (r1: [0--0.5) ) (r2: [0.5--1) ) (g1: [0-0.3)[0.6-1.0) ) (g2: [0.3--0.6) )
    //         \             /                     \              /
    //          \           /                       \            /
    //          (l1: [0---1) )                      (l2: [0---1) )
  
    

    Node *c2 = arg->coalescence(0.0, r1, g2);

    // current ARG:
    //
    //                                    .--- retired
    //                                   /
    //                               .-------.
    //                .--(c2: [0-0.3)[0.3-0.5)[0.5-0.6) )---.
    //               /                                       \       --no nl esc
    //              /    (c1: [0-0.3)[0.5-0.6)[0.6-1.0) )     \      --no nl esc
    //             /                /    \    `------' retired \     --no nl esc
    //            /                /      \                     \    --no nl esc
    //  (r1: [0--0.5) ) (r2: [0.5--1) ) (g1: [0-0.3)[0.6-1.0) ) (g2: [0.3--0.6) )
    //         \             /                     \              /
    //          \           /                       \            /
    //          (l1: [0---1) )                      (l2: [0---1) )



    // top node collecting the rest
    arg->coalescence(0.0, c1, c2);

    return arg;
}

int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {

	std::vector<Marker*> markers;
	markers.push_back(new SNPMarker(0.0, 0.0,1.0));
	markers.push_back(new TraitMarker(0.2, 0.0,1.0));
	markers.push_back(new SNPMarker(0.3, 0.0,1.0));
	MicroSatelliteMarker *mm = new MicroSatelliteMarker(0.4, 5.0);
	mm->add_value(4); mm->add_value(8);
	markers.push_back(mm);
	markers.push_back(new SNPMarker(0.67, 0.0,1.0));

	Configuration conf(2,
			   markers.begin(), markers.end(),
			   5.0, 250.0, 250.0, 5.0);

	// FIXME: not completely exception safe -- memory leak if
	// exception thrown... who cares, it's only a test after all...
	std::vector<Marker*>::iterator i;
	for (i = markers.begin(); i != markers.end(); ++i)
	    delete *i;
	markers.resize(0);


	Descender desc(conf);

	ARG *arg = build_arg(conf);
	desc.evolve(*arg, 0);


    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    REPORT_RESULTS;
}
