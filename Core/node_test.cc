/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "testing.hh"

#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef SNP_MARKER_HH_INCLUDED
# include "snp_marker.hh"
#endif

#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif

int main(int argc, const char *argv[])
{
    HANDLE_TEST_OPTIONS;

    try {

	std::vector<Marker*> markers;
	std::vector<Marker*>::iterator i;

	markers.push_back(new SNPMarker(0.0, 0.0,1.0));
	markers.push_back(new SNPMarker(0.2, 0.0,1.0));
	markers.push_back(new SNPMarker(0.3, 0.0,1.0));
	markers.push_back(new SNPMarker(0.4, 0.0,1.0));
	markers.push_back(new SNPMarker(0.67, 0.0,1.0));

	Configuration conf(0,
			   markers.begin(), markers.end(),
			   0.0, 0.0, 0.0, 0.0);

    
	for (i = markers.begin(); i != markers.end(); ++i)
	    delete *i;
	markers.resize(0);
	

	ARG arg(conf);

	Node *l1 = arg.leaf();
	CHECK(l1 != 0);
	Node *l2 = arg.leaf();
	CHECK(l2 != 0);

	// current ARG:
	//
	//   (l1: [0---1) )    (l2: [0---1) )

	CHECK(l1->intervals().interval(0).leaf_contacts() == 1);
	CHECK(l2->intervals().interval(0).leaf_contacts() == 1);

	CHECK(l1->leaves_at_point(0.5) == 1);
	CHECK(l2->leaves_at_point(0.5) == 1);

	CHECK(l1->intervals().contains_point(0.0));
	CHECK(l1->intervals().first_point() == 0.0);
	CHECK(l1->intervals().last_point() == 1.0);

	CHECK(l2->intervals().contains_point(0.0));
	CHECK(l2->intervals().first_point() == 0.0);
	CHECK(l2->intervals().last_point() == 1.0);

	for (int i = 0; i < conf.no_markers(); ++i)
	    CHECK(l1->intervals().contains_point(conf.position(i)));

	for (int i = 0; i < conf.no_markers(); ++i)
	    CHECK(l2->intervals().contains_point(conf.position(i)));

	for (int i = 0; i < conf.no_markers(); ++i)
	    CHECK(l1->surface_at_point(conf.position(i)) == 0.0);
	for (int i = 0; i < conf.no_markers(); ++i)
	    CHECK(l2->surface_at_point(conf.position(i)) == 0.0);


	ARG::node_pair_t p;

	p = arg.recombination(0.0,l1,0.0);
	CHECK(p.first == l1);
	CHECK(p.second == 0);

	p = arg.recombination(0.0,l1,1.0);
	CHECK(p.first == l1);
	CHECK(p.second == 0);

	p = arg.recombination(1.0,l1,0.5);
	CHECK(p.first != 0);
	CHECK(p.second != 0);

	Node *r1 = p.first;
	Node *r2 = p.second;

	// current ARG:
	//
	//  (r1: [0--0.5) )  (r2: [0.5--1) )
	//        \                /
	//     1.0 \              / 1.0
	//          \            /
	//          (l1: [0---1) )                  (l2: [0---1) )

	CHECK(r1->intervals().interval(0).leaf_contacts() == 1);

	CHECK(r1->intervals().size() == 1);
	CHECK(r1->intervals().first_point() == 0.0);
	CHECK(r1->intervals().last_point()  == 0.5);
	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.5)
		CHECK(r1->intervals().contains_point(conf.position(i)));

	CHECK(r2->intervals().interval(0).leaf_contacts() == 1);

	CHECK(r2->intervals().size() == 1);
	CHECK(r2->intervals().first_point() == 0.5);
	CHECK(r2->intervals().last_point()  == 1.0);
	for (int i = 0; i < conf.no_markers(); ++i)
	    if (0.5 <= conf.position(i))
		CHECK(r2->intervals().contains_point(conf.position(i)));

	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.5)
		{
		    CHECK(r1->intervals().contains_point(conf.position(i)));
		}
	    else
		{
		    CHECK(r2->intervals().contains_point(conf.position(i)));
		}

	// don't call "surface_at_point" for nodes not containing the
	// interval -- that is no longer valid as the invariant is that the
	// _caller_ checks that.  The reason for this is that the parent
	// must check that for its children anyway.

	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.5) 
		CHECK(r1->surface_at_point(conf.position(i)) == 1.0)
	    else 
		CHECK(!r1->intervals().contains_point(conf.position(i)))

	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.5) 
		CHECK(!r2->intervals().contains_point(conf.position(i)))
	    else
		CHECK(r2->surface_at_point(conf.position(i)) == 1.0)


	CHECK(r1->leaves_at_point(0.2) == 1);
	CHECK(r1->leaves_at_point(0.5) == 0);
	CHECK(r2->leaves_at_point(0.2) == 0);
	CHECK(l2->leaves_at_point(0.5) == 1);
	CHECK(l2->leaves_at_point(0.6) == 1);


	p = arg.recombination(0.0, r2, 0.25);
	CHECK(p.first == r2);
	CHECK(p.second == 0);

	p = arg.recombination(0.0, r2, 0.5);
	CHECK(p.first == r2);
	CHECK(p.second == 0);

	p = arg.recombination(0.0, r1, 0.5);
	CHECK(p.first == r1);
	CHECK(p.second == 0);

	p = arg.recombination(0.0, r1, 0.75);
	CHECK(p.first == r1);
	CHECK(p.second == 0);

	p = arg.gene_conversion(0.0, r2, 0.25, 0.45);
	CHECK(p.first == r2);
	CHECK(p.second == 0);

	p = arg.gene_conversion(0.0, r2, 0.25, 0.50);
	CHECK(p.first == r2);
	CHECK(p.second == 0);

	p = arg.gene_conversion(2.0, l2, 0.30, 0.60);
	CHECK(p.first != 0);
	CHECK(p.second != 0);

	Node *g1 = p.first;
	Node *g2 = p.second;

	// current ARG:
	//                             (g1: [0-0.3)[0.6-1.0) )     (g2: [0.3--0.6) )
	//                                         \                      /
	//                                          \                    /
	//  (r1: [0--0.5) ) (r2: [0.5--1) )       2.0\                  /2.0
	//        \               /                   \                /
	//      1.0\          1.0/                     \              /
	//          \           /                       \            /
	//          (l1: [0---1) )                      (l2: [0---1) )

	CHECK(g1->intervals().interval(0).leaf_contacts() == 1);
	CHECK(g1->intervals().interval(1).leaf_contacts() == 1);

	CHECK(g1->intervals().size() == 2);
	CHECK(g1->intervals().first_point() == 0.0);
	CHECK(g1->intervals().last_point()  == 1.0);

	CHECK(g2->intervals().interval(0).leaf_contacts() == 1);

	CHECK(g2->intervals().size() == 1);
	CHECK(g2->intervals().first_point() == 0.3);
	CHECK(g2->intervals().last_point()  == 0.6);


	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.3 or 0.6 <= conf.position(i))
		{
		    CHECK(g1->intervals().contains_point(conf.position(i)));
		}
	    else
		{
		    CHECK(g2->intervals().contains_point(conf.position(i)));
		}

	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.3 or 0.6 <= conf.position(i))
		{
		    CHECK(g1->surface_at_point(conf.position(i)) == 2.0);
		}
	    else
		{
		    CHECK(!g1->intervals().contains_point(conf.position(i)));
		}

	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.3 or 0.6 <= conf.position(i))
		{
		    CHECK(!g2->intervals().contains_point(conf.position(i)));
		}
	    else
		{
		    CHECK(g2->surface_at_point(conf.position(i)) == 2.0);
		}

	CHECK(g1->leaves_at_point(0.2) == 1);
	CHECK(g1->leaves_at_point(0.5) == 0);
	CHECK(g1->leaves_at_point(0.6) == 1);
	CHECK(g2->leaves_at_point(0.2) == 0);
	CHECK(g2->leaves_at_point(0.5) == 1);
	CHECK(g2->leaves_at_point(0.6) == 0);


	Node *c1 = arg.coalescence(3.0, r2, g1);
	CHECK(c1 != 0);

	// current ARG:
	// 
	// t=3                 (c1: [0-0.3)[0.5-0.6)[0.6-1.0) )
	//                                /    \    `-------' <- retired
	//                               /      \                         --no nl esc
	// t=2                          /  (g1: [0-0.3)[0.6-1.0) ) (g2: [0.3--0.6) )
	//                             /               \                  /
	// t=1 (r1: [0--0.5) ) (r2: [0.5--1) )          \                /
	//           \             /                     \              /
	//            \           /                       \            /
	// t=0        (l1: [0---1) )                      (l2: [0---1) )
  
    

	CHECK(c1->intervals().size() == 2);
	CHECK(arg.retired_intervals().size() == 1);
	CHECK(c1->intervals().first_point() == 0.0);
	CHECK(c1->intervals().last_point()  == 0.6);

	CHECK(c1->intervals().interval(0).leaf_contacts() == 1);
	CHECK(c1->intervals().interval(1).leaf_contacts() == 1);

	CHECK(arg.retired_intervals().at(0).top_node() == c1);

	CHECK(arg.retired_intervals().at(0).start() == 0.6);
	CHECK(arg.retired_intervals().at(0).end()   == 1.0);
	CHECK(arg.retired_intervals().at(0).leaf_contacts() == 2);
	CHECK(arg.retired_intervals().at(0).top_node() == c1);


	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.3 or (0.5 <= conf.position(i) and conf.position(i) < 0.6))
		{
		    CHECK(c1->intervals().contains_point(conf.position(i)));
		}
	    else if (0.6 <= conf.position(i))
		{
		    CHECK(arg.retired_intervals().at(0).contains_point(conf.position(i)));
		}

	CHECK(c1->surface_at_point(0.0) == 3.0);
	CHECK(c1->surface_at_point(0.5) == 3.0);
	CHECK(c1->surface_at_point(0.6) == 6.0);
	CHECK(arg.retired_intervals().at(0).surface() == 6.0);

	Node *c2 = arg.coalescence(4.0, r1, g2);
	CHECK(c1 != 0);

	// current ARG:
	//
	//                                    .--- retired
	//                                   /
	//                               .-------.
	// t=4            .--(c2: [0-0.3)[0.3-0.5)[0.5-0.6) )---.
	//               /                                       \       --no nl esc
	// t=3          /    (c1: [0-0.3)[0.5-0.6)[0.6-1.0) )     \      --no nl esc
	//             /                /    \    `------' retired \     --no nl esc
	//            /                /      \                     \    --no nl esc
	//  (r1: [0--0.5) ) (r2: [0.5--1) ) (g1: [0-0.3)[0.6-1.0) ) (g2: [0.3--0.6) )
	//         \             /                     \              /
	//          \           /                       \            /
	//          (l1: [0---1) )                      (l2: [0---1) )


	CHECK(c2->intervals().size() == 2);
	CHECK(arg.retired_intervals().size() == 2);

	CHECK(c2->intervals().first_point() == 0.0);
	CHECK(c2->intervals().last_point()  == 0.6);

	CHECK(c2->intervals().interval(0).leaf_contacts() == 1);
	CHECK(c2->intervals().interval(1).leaf_contacts() == 1);

	CHECK(arg.retired_intervals().at(1).start() == 0.3);
	CHECK(arg.retired_intervals().at(1).end()   == 0.5);
	CHECK(arg.retired_intervals().at(1).leaf_contacts() == 2);
	CHECK(arg.retired_intervals().at(1).top_node() == c2);


	for (int i = 0; i < conf.no_markers(); ++i)
	    if (conf.position(i) < 0.3 or (0.5 <= conf.position(i) and conf.position(i) < 0.6))
		{
		    CHECK(c2->intervals().contains_point(conf.position(i)));
		}
	    else if (0.3 <= conf.position(i) and conf.position(i) < 0.5)
		{
		    CHECK(arg.retired_intervals().at(1).contains_point(conf.position(i)));
		}


	CHECK(c2->surface_at_point(0.00) == 4.0);
	CHECK(c2->surface_at_point(0.33) == 8.0);
	CHECK(c2->surface_at_point(0.55) == 4.0);
	CHECK(arg.retired_intervals().at(1).surface() == 8.0);


	Node *top = arg.coalescence(5.0, c1, c2);
	CHECK(top != 0);

	CHECK(top->intervals().size() == 0);
	CHECK(arg.retired_intervals().size() == 4);

	// retired intervals: [0-0.3)[0.3-0.5)[0.5-0.6)[0.6-1)

	CHECK(arg.retired_intervals().at(2).start() == 0.0);
	CHECK(arg.retired_intervals().at(2).end()   == 0.3);
	CHECK(arg.retired_intervals().at(2).leaf_contacts() == 2);
	CHECK(arg.retired_intervals().at(2).top_node() == top);

	CHECK(arg.retired_intervals().at(3).start() == 0.5);
	CHECK(arg.retired_intervals().at(3).end()   == 0.6);
	CHECK(arg.retired_intervals().at(3).leaf_contacts() == 2);
	CHECK(arg.retired_intervals().at(3).top_node() == top);

	CHECK(arg.retired_intervals().at(0).surface() ==  6.0);
	CHECK(arg.retired_intervals().at(1).surface() ==  8.0);
	CHECK(arg.retired_intervals().at(2).surface() == 10.0);
	CHECK(arg.retired_intervals().at(3).surface() == 10.0);


	try {
	    arg.coalescence(0.0, 0, 0);
	    ERROR("coalecense with null-children");
	} catch (ARG::null_child&) {}
	try {
	    arg.coalescence(0.0, l1, 0);
	    ERROR("coalecense with null-children");
	} catch (ARG::null_child&) {}
	try {
	    arg.coalescence(0.0, 0, l2);
	    ERROR("coalecense with null-children");
	} catch (ARG::null_child&) {}

	try {
	    arg.recombination(0.0, 0, 0.0);
	    ERROR("recombination with null-children");
	} catch (ARG::null_child&) {}

	try {
	    arg.gene_conversion(0.0, 0, 0.0, 0.2);
	    ERROR("gene-conversion with null-children");
	} catch (ARG::null_child&) {}







#if 0 // won't work right now, 'cause the markers aren't initialized
	std::ofstream xml_file("node_test.xml");
	xml_file << arg;
	xml_file.close();
#endif


    } catch (std::exception &ex) {
	std::cout << "EXCEPTION: " << ex.what() << std::endl;
	return 2;
    }

    REPORT_RESULTS;
}
