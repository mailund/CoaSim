
#include "testing.hh"

#include "simulator.hh"

#include "configuration.hh"
#include "all_markers.hh"

/*
 * WARNING: THIS PROGRAM IS NOT REALLY TESTING SIMULATOR -- WE NEED TO
 * DO THAT STATISTICALLY SOMEHOW -- THE PROGRAM SIMPLY TRIES TO RUN
 * THE SIMULATOR (TO SEE THAT IT DOESN'T CRASH).
 */


int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  try {

    const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
    const size_t no_positions = (sizeof positions)/sizeof(double);

    const unsigned int no_leaves = 2;
    const double rho = 1.0;	// 5.0
    const double Q = 0.0;	// 250.0
    const double G = 0.0;	// 250.0
    const double growth = 5.0;
    const double mu = 5.0;

    Configuration conf(no_leaves,
		       (const double*)positions, &positions[no_positions],
		       rho, Q, G, growth, true);

    TraitMarker          trait_m(0.1,0.2); // 10%-20%
    SNPMarker            snp_m  (0.1,0.2); // 10%-20%
    MicroSatelliteMarker ms_m(mu); ms_m.add_value(4); ms_m.add_value(8);

    conf.set_marker(0,&snp_m);
    conf.set_marker(1,&trait_m);
    conf.set_marker(2,&snp_m);
    conf.set_marker(3,&ms_m);
    conf.set_marker(4,&snp_m);

    ARG *arg = Simulator::simulate(conf);
    CHECK(arg != 0);


  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }

  REPORT_RESULTS;
}
