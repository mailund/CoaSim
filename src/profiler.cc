
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
#include <popt.h>


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

#if 0
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
#endif

  std::vector<double> positions;
  for (itr = markers.begin(); itr != markers.end(); ++itr)
    positions.push_back(itr->position);

  Configuration conf(no_leaves,
		     positions.begin(), positions.end(),
		     rho, Q, G, growth);

  TraitMarker          trait_m(0.1,0.2); // 10%-20%
  SNPMarker            snp_m  (0.1,0.2); // 10%-20%
  MicroSatelliteMarker ms_m(mu); ms_m.add_value(4); ms_m.add_value(8);

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
		      
namespace options {
  int no_leaves;

  int no_traits;
  int no_snps;
  int no_ms;

  double rho;
  double Q;
  double G;
  double growth;
  double mu;
}

static struct poptOption cl_options[] = {
    {
      "leaves",
      'l',
      POPT_ARG_INT,
      &options::no_leaves,
      0,
      "Set number of leaves.",
      "no-leaves"
    },

    {
      "traits",
      '\0',
      POPT_ARG_INT,
      &options::no_traits,
      0,
      "Set number of trait markers.",
      "no-traits"
    },
    {
      "snps",
      '\0',
      POPT_ARG_INT,
      &options::no_snps,
      0,
      "Set number of SNP markers.",
      "no-snps"
    },
    {
      "ms",
      '\0',
      POPT_ARG_INT,
      &options::no_ms,
      0,
      "Set number of micro sattelite markers.",
      "no-mss"
    },

    {
      "rho",
      '\0',
      POPT_ARG_DOUBLE,
      &options::rho,
      0,
      "Set rho.",
      "rho"
    },
    {
      "Q",
      'Q',
      POPT_ARG_DOUBLE,
      &options::Q,
      0,
      "Set Q.",
      "Q"
    },
    {
      "G",
      'G',
      POPT_ARG_DOUBLE,
      &options::G,
      0,
      "Set G.",
      "G"
    },
    {
      "growth",
      '\0',
      POPT_ARG_DOUBLE,
      &options::growth,
      0,
      "Set growth.",
      "growth"
    },
    {
      "mu",
      '\0',
      POPT_ARG_DOUBLE,
      &options::mu,
      0,
      "Set mu.",
      "mu"
    },


    POPT_AUTOHELP
    { 0 } // sentinel
};

int main(int argc, const char *argv[])
{

  poptContext ctxt = poptGetContext(0, argc, argv, cl_options, 0);
  int opt = poptGetNextOpt(ctxt);
  if (opt < -1)
    {
      std::cerr << poptBadOption(ctxt, POPT_BADOPTION_NOALIAS)
		<< ':' << poptStrerror(opt) << std::endl;
      return 2;
    }

  if (!options::no_leaves)
    {
      std::cerr << "number of leaves not specified!\n";
      return 2;
    }


  try {
    time_t runtime = run_simulation(options::no_leaves,

				    options::no_traits,
				    options::no_snps,
				    options::no_ms,

				    options::rho,
				    options::Q,
				    options::G,
				    options::growth,
				    options::mu);

    std::cout << "test took " << runtime << " seconds\n";

  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
    return 2;
  }
}
