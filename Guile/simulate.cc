/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "simulate.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__ARG_PARAMETERS_HH_INCLUDED
# include "arg_parameters.hh"
#endif
#ifndef GUILE__OPTIONS_HH_INCLUDED
# include "options.hh"
#endif

using namespace guile;


#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif
#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include <Core/configuration.hh>
#endif
#ifndef CORE__SIMULATOR_HH_INCLUDED
# include <Core/simulator.hh>
#endif
#ifndef CORE__MONITOR_HH_INCLUDED
# include <Core/monitor.hh>
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif


#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_H_INCLUDED
#endif
#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif


scm_t_bits guile::arg_tag;

namespace {
    struct ARGData {
	const core::ARG *arg;
	const core::Configuration *conf;
	ARGData(core::ARG *arg, core::Configuration *conf)
	    : arg(arg), conf(conf)
	{};
	~ARGData()
	{
	    delete arg;
	    delete conf;
	}
    };
}

static size_t
free_arg(SCM s_arg_data)
{
    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(s_arg_data);
    arg_data->~ARGData();
    scm_must_free(arg_data);
    return sizeof(ARGData);
}

namespace {
    class CLISimMonitor : public core::SimulationMonitor {
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


static SCM
simulate(SCM arg_parameters_smob, SCM s_markers, SCM s_no_leaves)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_parameters_tag, 
				  arg_parameters_smob),
	       arg_parameters_smob, SCM_ARG1, "simulate");
    SCM_ASSERT(SCM_NFALSEP(scm_list_p(s_markers)),
	       s_markers, SCM_ARG2, "simulate");

    ARGParameters *p = (ARGParameters*) SCM_SMOB_DATA(arg_parameters_smob);

    SCM itr_markers = s_markers;
    std::vector<core::Marker*> markers;
    while (!SCM_NULLP(itr_markers))
	{
	    SCM marker_smob = SCM_CAR(itr_markers);
	    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::trait_marker_tag, marker_smob)
		       or
		       SCM_SMOB_PREDICATE(guile::snp_marker_tag, marker_smob)
		       or
		       SCM_SMOB_PREDICATE(guile::ms_marker_tag, marker_smob),
		       marker_smob, SCM_ARG2, "simulate");

	    core::Marker *marker = (core::Marker*) SCM_SMOB_DATA(marker_smob);
	    markers.push_back(marker);

	    itr_markers = SCM_CDR(itr_markers);
	}

    int no_leaves = scm_num2int(s_no_leaves, SCM_ARG3, "simulate");


    try {
	CLISimMonitor mon;
	core::Configuration *conf 
	    = new core::Configuration(no_leaves,
				      markers.begin(), markers.end(),
				      p->rho, p->Q, p->G, p->growth);
	core::ARG *arg = core::Simulator::simulate(*conf,
						   options::verbose ? &mon:0);

	void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
	ARGData *arg_data = new(mem)ARGData(arg,conf);
    
	SCM_RETURN_NEWSMOB(guile::arg_tag, arg_data);
    } catch(core::Configuration::out_of_sequence&) {
	scm_throw(scm_str2symbol("out-of-sequence"), s_markers);
    } catch(std::exception &ex) {
	scm_throw(scm_str2symbol("unexcepted-exception"), 
		  scm_mem2string(ex.what(),strlen(ex.what())));
    }

    // shouldn't really reach...
    return SCM_EOL;
}

static SCM 
save_sequences(SCM arg_data_smob, SCM s_filename)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "save-sequences");
    SCM_ASSERT(SCM_NFALSEP(scm_string_p(s_filename)),
	       s_filename, SCM_ARG2, "save-sequences");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);
    const char *fname = SCM_STRING_CHARS(s_filename);

    std::ofstream os(fname);
    if (!os) scm_throw(scm_str2symbol("open-error"), s_filename);

    arg_data->arg->to_text(os);
    os.close();

    return SCM_EOL;
}

static SCM 
sequences(SCM arg_data_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "save-sequences");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);

    SCM sequences = SCM_EOL;

    const std::vector<core::Node*> &leaves = arg_data->arg->leaves();
    std::vector<core::Node*>::const_iterator i;
    for (i = leaves.begin(); i != leaves.end(); ++i)
	{
	    SCM seq = SCM_EOL;
	    for (size_t j = 0; j < (*i)->no_states(); ++j)
		seq = scm_cons(scm_int2num((*i)->state(j)), seq);
	    sequences = scm_cons(scm_reverse(seq), sequences);
	}

    return scm_reverse(sequences);
}


void
guile::install_simulate()
{
    guile::arg_tag = scm_make_smob_type("arg", sizeof(ARGData));
    scm_set_smob_free(guile::arg_tag, free_arg);

    scm_c_define_gsubr("simulate", 3, 0, 0, 
		       (scm_unused_struct*(*)())simulate);
    scm_c_define_gsubr("save-sequences", 2, 0, 0, 
		       (scm_unused_struct*(*)())save_sequences);
    scm_c_define_gsubr("sequences", 1, 0, 0, 
		       (scm_unused_struct*(*)())sequences);
}


