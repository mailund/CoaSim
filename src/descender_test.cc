

#include "descender.hh"
#include "configuration.hh"
#include "node.hh"
#include "all_markers.hh"

#include "testing.hh"

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

    const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
    const size_t no_positions = (sizeof positions)/sizeof(double);


    Configuration conf(2,
		       (const double*)positions, &positions[no_positions],
		       5.0, 250.0, 250.0, 5.0, true);

    TraitMarker          trait_m(0.0,1.0);
    SNPMarker            snp_m(0.0,1.0);
    MicroSatelliteMarker ms_m(5.0); ms_m.add_value(4); ms_m.add_value(8);

    conf.set_marker(0,&snp_m);
    conf.set_marker(1,&trait_m);
    conf.set_marker(2,&snp_m);
    conf.set_marker(3,&ms_m);
    conf.set_marker(4,&snp_m);

    Descender desc(conf);

    ARG *arg = build_arg(conf);
    desc.evolve(*arg);


  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }

  REPORT_RESULTS;
}
