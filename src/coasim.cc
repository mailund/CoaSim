
#ifndef CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef ALL_MARKERS_HH_INCLUDED
# include "all_markers.hh"
#endif
#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef SIMULATOR_HH_INCLUDED
# include "simulator.hh"
#endif
#ifndef RC_PARSER_HH_INCLUDED
# include "rc-parser.hh"
#endif
#ifndef MONITOR_HH_INCLUDED
# include "monitor.hh"
#endif

#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif

#ifndef SYS_TIME_H_INCLUDED
# include <sys/time.h>
# define SYS_TIME_H_INCLUDED
#endif
#ifndef POPT_H_INCLUDED
# include <popt.h>
# define POPT_H_INCLUDED
#endif

using namespace std;

namespace options {
  int verbose;
  int print_full_arg;
  int print_xml;

  const char *rc_file;
  const char *output_file;
}

static struct poptOption cl_options[] = {
    {
      "verbose",
      'v',
      POPT_ARG_NONE,
      &options::verbose,
      0,
      "Toggle verbose output.",
      0
    },
    {
      "run-commands",
      'r',
      POPT_ARG_STRING,
      &options::rc_file,
      0,
      "Configuration file.",
      "rcfile"
    },
    {
      "output-file",
      'o',
      POPT_ARG_STRING,
      &options::output_file,
      0,
      "Output file.",
      "outfile"
    },
    {
      "print-xml",
      'x',
      POPT_ARG_NONE,
      &options::print_xml,
      0,
      "Write output as XML as opposed to plain text.  The XML output "
      "contains a lot of details not found in the plain text output.",
      0
    },
    {
      "print-full-arg",
      '\0',
      POPT_ARG_NONE,
      &options::print_full_arg,
      0,
      "Write the full simulated ARG to the output file, rather than just the "
      "leaf-nodes.  This flag is only used for XML output.",
      0
    },

    POPT_AUTOHELP
    { 0 } // sentinel
};

namespace {
  class CLISimMonitor : public SimulationMonitor {
    void start_arg_building(unsigned int no_leaves);
    void builder_update(unsigned int no_nodes, unsigned int no_top_nodes,
			unsigned long int no_iterations, double cur_time,
			unsigned int no_coal_events,
			unsigned int no_gene_conv_events,
			unsigned int no_recomb_events);
    void builder_termination(unsigned int no_nodes, unsigned int no_top_nodes,
			     unsigned long int no_iterations, double cur_time,
			     unsigned int no_coal_events,
			     unsigned int no_gene_conv_events,
			     unsigned int no_recomb_events);

    void start_mutating();
    void mutator_update(unsigned int marker_no);
    void retry_mutation();
    void retry_arg_building();

    void simulation_terminated();
  };
}
void CLISimMonitor::start_arg_building(unsigned int no_leaves)
{
  std::cout << "START BUILDING ARG...\n";
}

void CLISimMonitor::builder_update(unsigned int no_nodes,
				   unsigned int no_top_nodes,
				   unsigned long int no_iterations, 
				   double cur_time,
				   unsigned int no_coal_events,
				   unsigned int no_gene_conv_events,
				   unsigned int no_recomb_events)
{
  std::cout << "Iteration: " << no_iterations
	    << " time " << cur_time << '\n'
	    << no_nodes << " nodes in ARG, "
	    << no_top_nodes << " remaining to be processed.\n"
	    << '\t' << no_coal_events << " coalescence events\n"
	    << '\t' << no_gene_conv_events << " gene conversion events\n"
	    << '\t' << no_recomb_events << " recombination events\n";
}

void CLISimMonitor::builder_termination(unsigned int no_nodes,
					unsigned int no_top_nodes,
					unsigned long int no_iterations,
					double cur_time,
					unsigned int no_coal_events,
					unsigned int no_gene_conv_events,
					unsigned int no_recomb_events)
{
  std::cout << "\nARG Building terminated after " << no_iterations 
	    << " iterations at time " << cur_time << '\n'
	    << no_nodes << " nodes in ARG, "
	    << no_top_nodes << " remaining to be processed.\n"
	    << '\t' << no_coal_events << " coalescence events\n"
	    << '\t' << no_gene_conv_events << " gene conversion events\n"
	    << '\t' << no_recomb_events << " recombination events\n\n";
}

void CLISimMonitor::start_mutating()
{
  std::cout << "START MUTATING ARG...\n";
}

void CLISimMonitor::mutator_update(unsigned int marker_no)
{
  std::cout << "Mutating marker " << marker_no << "...\n";
}

void CLISimMonitor::retry_mutation()
{
  std::cout << "\tmutation not withing bounds, retrying...\n";
}

void CLISimMonitor::retry_arg_building()
{
  std::cout << "\tmutation not withing bounds of trait marker\n"
	    << "\tbuilding new ARG...\n\n";
}

void CLISimMonitor::simulation_terminated()
{
  std::cout << "\nSIMULATION COMPLETED\n";
}

static void set_markers(Configuration &conf,
			vector<string> &markers,
			vector<double> &low_freq,
			vector<double> &high_freq,
			vector<int> &no_values,
			vector<double> &mutation_rates)
{
  unsigned int i;
  for (i = 0; i < markers.size(); ++i)
    {
      if (markers[i] == "trait")
	{
	  TraitMarker *tm = new TraitMarker(low_freq[i],high_freq[i]);
	  conf.set_marker(i,tm,true);
	}
      else if (markers[i] == "snp")
	{
	  SNPMarker *sm = new SNPMarker(low_freq[i],high_freq[i]);
	  conf.set_marker(i,sm);
	}
      else if (markers[i] == "ms")
	{
	  if (no_values[i] < 2)
	    {
	      std::cerr << "At least 2 values should be possible for "
			<< "micro-satttelite at marker " << i << std::endl;
	      exit(2);
	    }
	  double mu = mutation_rates[i];
	  MicroSatelliteMarker *mm = new MicroSatelliteMarker(mu);
	  for (int j = 0; j < no_values[i]; ++j)
	    mm->add_value(j);
	  conf.set_marker(i,mm);
	}
      else
	{
	  std::cerr << "Error, unknown marker type: "
		    << markers[i] << std::endl;
	  exit(2);
	}
    }
}


static Configuration *parse_rc(const char *rc_file)
{
  std::ifstream is(rc_file);
  RCParser rcp(is);

  int no_leaves;

  double gene_conversion_rate, gene_conversion_length;
  double recombination_rate;
  double growth;

  vector<double> positions;
  vector<string> markers;
  vector<double> low_freq;
  vector<double> high_freq;
  vector<int>    no_values;
  vector<double> mutation_rates;

  no_leaves = rcp.get_int("no_leaves");

  gene_conversion_rate = rcp.get_double("gene_conversion_rate");
  gene_conversion_length = rcp.get_double("gene_conversion_length");
  recombination_rate = rcp.get_double("recombination_rate");
  growth = rcp.get_double("growth");

  positions = rcp.get_double_vector("positions");
  markers = rcp.get_string_vector("markers");
  low_freq = rcp.get_double_vector("low_freq");
  high_freq = rcp.get_double_vector("high_freq");
  no_values = rcp.get_int_vector("no_values");
  mutation_rates = rcp.get_double_vector("mutation_rates");

  // test the input a bit...
  if (no_leaves < 1)
    {
      std::cerr << "At least 1 leaf node should be generated!\n";
      exit(2);
    }

  if (positions.size() != markers.size())
    {
      std::cerr << "There should be as many markers as there are positions\n";
      exit(2);
    }

  if (positions.size() != low_freq.size())
    {
      std::cerr << "There should be as many low-freq as there are markers\n";
      exit(2);
    }

  if (positions.size() != high_freq.size())
    {
      std::cerr << "There should be as many high-freq as there are markers\n";
      exit(2);
    }

  if (positions.size() != no_values.size())
    {
      std::cerr << "There should be as many no_values as there are markers\n";
      exit(2);
    }

  if (positions.size() != mutation_rates.size())
    {
      std::cerr << "There should be as many mutation rates as there are markers\n";
      exit(2);
    }


  SimulationMonitor *mon = 0;
  if (options::verbose) mon = new CLISimMonitor();

  Configuration *conf = new Configuration(no_leaves,
					  positions.begin(), positions.end(),
					  recombination_rate,
					  gene_conversion_rate,
					  gene_conversion_length,
					  growth,
					  mon);

  set_markers(*conf, markers, low_freq, high_freq, no_values, mutation_rates);

  return conf;
}



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

  if (!options::rc_file)
    {
      std::cerr << "Error: no run-command specified!\n";
      return 2;
    }

  if (!options::output_file)
    {
      std::cerr << "Error: no output file specified!\n";
      return 2;
    }


  // set rand seed
  struct timeval tv; struct timezone tz;
  gettimeofday(&tv,&tz);
  std::srand(tv.tv_usec);

  try {
    std::ofstream out(options::output_file);
    Configuration *conf = parse_rc(options::rc_file);
    ARG *arg = Simulator::simulate(*conf);
    if (!arg)
      {
	std::cout << "Simulation aborted!\n";
	return 0;
      }

    if (options::print_xml)
      arg->to_xml(out, options::print_full_arg);
    else
      arg->to_text(out);

  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
    return 2;
  }
}
