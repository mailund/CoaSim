
#include "testing.hh"

#include "configuration.hh"
#include "node.hh"
#include "all_markers.hh"
#include "test_dist_funcs.hh"



int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  try {

    const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
    const size_t no_positions = (sizeof positions)/sizeof(double);
    Configuration conf((const double*)positions, &positions[no_positions],
		       0.0, 0.0, 0.0, 0.0, 0.0, true);

    ARG arg(conf);

    Node *l1 = arg.leaf();
    Node *l2 = arg.leaf();
    ARG::node_pair_t p;
    p = arg.recombination(1.0,l1,0.5);
    Node *r1 = p.first;
    Node *r2 = p.second;
    p = arg.gene_conversion(2.0, l2, 0.30, 0.60);
    Node *g1  = p.first;
    Node *g2  = p.second;
    Node *c1  = arg.coalescence(3.0, r2, g1);
    Node *c2  = arg.coalescence(4.0, r1, g2);
    Node *top = arg.coalescence(5.0, c1, c2);

    // ARG retired intervals:
    // 0: [0.6--1.0)  s =  6.0   markers: 4
    // 1: [0.3--0.5)  s =  8.0   markers: 2 3
    // 2: [0.0--0.3)  s = 10.0   markers: 0 1
    // 3: [0.5--0.6)  s = 10.0   markers:

    // Tree for interval 0:
    //
    //          c1
    //         /  \ 1.0
    //    2.0 /    g1
    //       /      |
    //      r2      | 2.0
    //  1.0 |       |
    //      l1      l2

    // SNP mutation, put mutation on c1->r1 edge
    SNPMarker snp_m;
    Distribution_functions::uniform_result = 0.0;
    Mutator *mutator = snp_m.create_mutator(arg.retired_intervals().at(0));

    c1->initialize_marker(4,snp_m);
    c1->mutate_marker(4,*mutator);
    delete mutator;

    CHECK(c1->state(4) == 0);
    CHECK(r2->state(4) == 1);
    CHECK(l1->state(4) == 1);
    CHECK(g1->state(4) == 0);
    CHECK(l2->state(4) == 0);

    // Tree for interval 1:
    // 
    //          c2
    //         /  \ 2.0
    //        /    |
    //   3.0 /     g2
    //       |      |
    //       r1     | 2.0
    //  1.0  |      |
    //       l1     l2



    // Tree for interval 2:
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
 
   


  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }

  REPORT_RESULTS;
}
