
#include "node.hh"
#include "testing.hh"
#include "dist_funcs.hh"

static Configuration *global_conf;

int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  try {

  const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
  const size_t no_positions = (sizeof positions)/sizeof(double);

  global_conf = new Configuration((const double*)positions,
				  &positions[no_positions]);

  ARG arg(*global_conf);

  ARG::Node *l1 = arg.leaf();
  CHECK(l1 != 0);
  ARG::Node *l2 = arg.leaf();
  CHECK(l2 != 0);

  // current ARG:
  //
  //   (l1: [0---1) )    (l2: [0---1) )

  CHECK(l1->intervals().interval(0).leaf_contacts() == 1);
  CHECK(l2->intervals().interval(0).leaf_contacts() == 1);

  CHECK(l1->intervals().is_start(0.0));
  CHECK(l1->intervals().is_end(1.0));
  CHECK(l1->intervals().first_point() == 0.0);
  CHECK(l1->intervals().last_point() == 1.0);

  CHECK(l2->intervals().is_start(0.0));
  CHECK(l2->intervals().is_end(1.0));
  CHECK(l2->intervals().first_point() == 0.0);
  CHECK(l2->intervals().last_point() == 1.0);



  ARG::node_pair_t p;

  p = arg.recombination(0.0,l1,0.0);
  CHECK(p.first == l1);
  CHECK(p.second == 0);

  p = arg.recombination(0.0,l1,1.0);
  CHECK(p.first == l1);
  CHECK(p.second == 0);

  p = arg.recombination(0.0,l1,0.5);
  CHECK(p.first != 0);
  CHECK(p.second != 0);

  ARG::Node *r1 = p.first;
  ARG::Node *r2 = p.second;

  // current ARG:
  //
  //  (r1: [0--0.5) )  (r2: [0.5--1) )
  //         \              /
  //          \            /
  //          (l1: [0---1) )                  (l2: [0---1) )

  CHECK(r1->intervals().interval(0).leaf_contacts() == 1);

  CHECK(r1->intervals().size() == 1);
  CHECK(r1->intervals().first_point() == 0.0);
  CHECK(r1->intervals().last_point()  == 0.5);
  for (size_t i = 0; i < no_positions; ++i)
    if (positions[i] < 0.5)
      CHECK(r1->intervals().contains_point(positions[i]));

  CHECK(r2->intervals().interval(0).leaf_contacts() == 1);

  CHECK(r2->intervals().size() == 1);
  CHECK(r2->intervals().first_point() == 0.5);
  CHECK(r2->intervals().last_point()  == 1.0);
  for (size_t i = 0; i < no_positions; ++i)
    if (0.5 <= positions[i])
      CHECK(r2->intervals().contains_point(positions[i]));






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


  p = arg.gene_conversion(0.0, r2, 0.25, 0.2);
  CHECK(p.first == r2);
  CHECK(p.second == 0);

  p = arg.gene_conversion(0.0, r2, 0.25, 0.25);
  CHECK(p.first == r2);
  CHECK(p.second == 0);

  p = arg.gene_conversion(0.0, l2, 0.30, 0.30);
  CHECK(p.first != 0);
  CHECK(p.second != 0);

  ARG::Node *g1 = p.first;
  ARG::Node *g2 = p.second;

  // current ARG:
  //
  //  (r1: [0--0.5) ) (r2: [0.5--1) ) (g1: [0-0.3)[0.6-1.0) ) (r2: [0.3--0.6) )
  //         \             /                     \              /
  //          \           /                       \            /
  //          (l1: [0---1) )                      (l2: [0---1) )

  CHECK(g1->intervals().interval(0).leaf_contacts() == 1);
  CHECK(g1->intervals().interval(1).leaf_contacts() == 1);

  CHECK(g1->intervals().size() == 2);
  CHECK(g1->intervals().first_point() == 0.0);
  CHECK(g1->intervals().last_point()  == 1.0);
  CHECK(g1->intervals().is_start(0.0));
  CHECK(g1->intervals().is_end  (0.3));
  CHECK(g1->intervals().is_start(0.6));
  CHECK(g1->intervals().is_end  (1.0));

  CHECK(g2->intervals().interval(0).leaf_contacts() == 1);

  CHECK(g2->intervals().size() == 1);
  CHECK(g2->intervals().first_point() == 0.3);
  CHECK(g2->intervals().last_point()  == 0.6);




  ARG::Node *c1 = arg.coalescence(0.0, r2, g1);
  CHECK(c1 != 0);

  // current ARG:
  // 
  //                   (c1: [0-0.3)[0.5-0.6)[0.6-1.0) )
  //                              /    \    `-------' <- retired
  //                             /      \                         --no nl esc
  //  (r1: [0--0.5) ) (r2: [0.5--1) ) (g1: [0-0.3)[0.6-1.0) ) (g2: [0.3--0.6) )
  //         \             /                     \              /
  //          \           /                       \            /
  //          (l1: [0---1) )                      (l2: [0---1) )
  
    

  CHECK(c1->intervals().size() == 2);
  CHECK(arg.retired_intervals().size() == 1);
  CHECK(c1->intervals().first_point() == 0.0);
  CHECK(c1->intervals().last_point()  == 0.6);
  CHECK(c1->intervals().is_start(0.0));
  CHECK(c1->intervals().is_end  (0.3));
  CHECK(c1->intervals().is_start(0.5));
  CHECK(c1->intervals().is_end  (0.6));

  CHECK(c1->intervals().interval(0).leaf_contacts() == 1);
  CHECK(c1->intervals().interval(1).leaf_contacts() == 1);

  CHECK(arg.retired_intervals().at(0).is_start(0.6));
  CHECK(arg.retired_intervals().at(0).is_end  (1.0));
  CHECK(arg.retired_intervals().at(0).leaf_contacts() == 2);


  ARG::Node *c2 = arg.coalescence(0.0, r1, g2);
  CHECK(c1 != 0);

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


  CHECK(c2->intervals().size() == 2);
  CHECK(arg.retired_intervals().size() == 2);

  CHECK(c2->intervals().first_point() == 0.0);
  CHECK(c2->intervals().last_point()  == 0.6);
  CHECK(c2->intervals().is_start(0.0));
  CHECK(c2->intervals().is_end  (0.3));
  CHECK(c2->intervals().is_start(0.5));
  CHECK(c2->intervals().is_end  (0.6));

  CHECK(c2->intervals().interval(0).leaf_contacts() == 1);
  CHECK(c2->intervals().interval(1).leaf_contacts() == 1);

  CHECK(arg.retired_intervals().at(1).is_start(0.3));
  CHECK(arg.retired_intervals().at(1).is_end  (0.5));
  CHECK(arg.retired_intervals().at(1).leaf_contacts() == 2);




  ARG::Node *top = arg.coalescence(0.0, c1, c2);
  CHECK(top != 0);

  CHECK(top->intervals().size() == 0);
  CHECK(arg.retired_intervals().size() == 4);

  // retired intervals: [0-0.3)[0.3-0.5)[0.5-0.6)[0.6-1)

  CHECK(arg.retired_intervals().at(2).is_start(0.0));
  CHECK(arg.retired_intervals().at(2).is_end  (0.3));
  CHECK(arg.retired_intervals().at(2).leaf_contacts() == 2);

  CHECK(arg.retired_intervals().at(3).is_start(0.5));
  CHECK(arg.retired_intervals().at(3).is_end  (0.6));
  CHECK(arg.retired_intervals().at(3).leaf_contacts() == 2);


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


  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }

  REPORT_RESULTS;
}
