
#ifndef CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef ALL_MARKERS_HH_INCLUDED
# include "all_markers.hh"
#endif
#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif

#ifndef BUILDER_HH_INCLUDED
# include "builder.hh"
#endif
#ifndef DESCENDER_HH_INCLUDED
# include "descender.hh"
#endif
#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif
#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif

#include <time.h>


enum marker_type_t { TRAIT, SNP, MS };
struct marker_t {
  marker_type_t type;
  double position;
  marker_t(marker_type_t type, double position) 
    : type(type), position(position)
  {}
  bool operator < (const marker_t &other) const 
  { return position < other.position; }
};

time_t run_simulation(unsigned int no_leaves,
		      unsigned int no_trait_markers,
		      unsigned int no_snp_markers,
		      unsigned int no_ms_markers,
		      double rho, double Q, double G, double growth, double mu)
{

  std::vector<marker_t> markers;
  for (unsigned int i = 0; i < no_trait_markers; ++i)
    {
      double pos = Distribution_functions::uniform();
      markers.push_back(marker_t(TRAIT,pos));
    }

  for (unsigned int i = 0; i < no_snp_markers; ++i)
    {
      double pos = Distribution_functions::uniform();
      markers.push_back(marker_t(SNP,pos));
    }

  for (unsigned int i = 0; i < no_ms_markers; ++i)
    {
      double pos = Distribution_functions::uniform();
      markers.push_back(marker_t(MS,pos));
    }

  std::sort(markers.begin(), markers.end());

  std::vector<marker_t>::const_iterator itr;
  int m;

  for (itr = markers.begin(), m = 0; itr != markers.end(); ++itr, ++m)
    {
      switch (itr->type)
	{
	case TRAIT:
	  std::cout << "marker " << m << " at pos " << itr->position
		    << " is a trait marker\n";
	  break;

	case SNP:
	  std::cout << "marker " << m << " at pos " << itr->position
		    << " is a snp marker\n";
	  break;

	case MS:
	  std::cout << "marker " << m << " at pos " << itr->position
		    << " is an ms marker\n";
	  break;
	}
    }
      

  std::vector<double> positions;
  for (itr = markers.begin(); itr != markers.end(); ++itr)
    positions.push_back(itr->position);

  Configuration conf(no_leaves,
		     positions.begin(), positions.end(),
		     rho, Q, G, growth, mu);

  TraitMarker          trait_m(0.1,0.2); // 10%-20%
  SNPMarker            snp_m  (0.1,0.2); // 10%-20%
  MicroSatelliteMarker ms_m(conf.mu()); ms_m.add_value(4); ms_m.add_value(8);

  for (m = 0, itr = markers.begin(); itr != markers.end(); ++itr, ++m)
    {
      switch (itr->type)
	{
	case TRAIT:
	  conf.set_marker(m,&trait_m);
	  break;

	case SNP:
	  conf.set_marker(m,&snp_m);
	  break;

	case MS:
	  conf.set_marker(m,&ms_m);
	  break;
	}
    }

  Builder builder(conf);
  Descender descender(conf);

  time_t start_time = time(0);

  ARG *arg = 0;
 retry:
  try {
    time_t start_time = time(0);
    arg = builder.build();
    time_t build_time = time(0) - start_time;

    std::cout << "building the arg took " << build_time << "sec.\n";

    start_time = time(0);
    descender.evolve(*arg);
    time_t mutation_time = time(0) - start_time;

    std::cout << "mutating the arg took " << mutation_time << "sec.\n";

  } catch (Mutator::retry_arg&) {
    std::cout << "RETRY ARG BUILDING...\n";
    goto retry;
  }

  return time(0) - start_time;
}
		      

int main(int argc, const char *argv[])
{
  try {

    // dummy test call
    time_t runtime = run_simulation(2, 1, 10, 10,
				    //5.0, 250.0, 20.0, 5.0, 5.0);
				    5.0, 250.0, 250.0, 5.0, 5.0);

    std::cout << "test took " << runtime << " seconds\n";

  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }
}
