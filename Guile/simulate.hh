/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef GUILE__SIMULATE_HH_INCLUDED
#define GUILE__SIMULATE_HH_INCLUDED

#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include <Core/configuration.hh>
#endif
#ifndef CORE__SIMULATOR_HH_INCLUDED
# include <Core/simulator.hh>
#endif
#ifndef CORE__BUILDER_HH_INCLUDED
# include <Core/builder.hh>
#endif
#ifndef CORE__MONITOR_HH_INCLUDED
# include <Core/monitor.hh>
#endif

#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {
    void install_simulate();
    extern scm_t_bits arg_tag;

    struct ProfileMonitor : public core::SimulationMonitor {

	unsigned int no_leaves;
	unsigned int no_nodes;
	unsigned int no_top_nodes;
	unsigned int no_iterations;
	double termination_time;
	unsigned int no_coal_events;
	unsigned int no_gene_conv_events;
	unsigned int no_recomb_events;
	
	ProfileMonitor()
	    : no_leaves(0), no_nodes(0), no_top_nodes(0),
	      no_iterations(0), termination_time(0),
	      no_coal_events(0), no_gene_conv_events(0), no_recomb_events(0)
	{}

	void start_arg_building(unsigned int no_leaves) 
	{
	    this->no_leaves = no_leaves;
	}
	void builder_update(unsigned int no_nodes, unsigned int no_top_nodes,
			    unsigned long int no_iterations, double cur_time,
			    unsigned int no_coal_events,
			    unsigned int no_gene_conv_events,
			    unsigned int no_recomb_events) {}
	void builder_termination(unsigned int no_nodes, unsigned int no_top_nodes,
				 unsigned long int no_iterations, double cur_time,
				 unsigned int no_coal_events,
				 unsigned int no_gene_conv_events,
				 unsigned int no_recomb_events);

	void start_mutating() {}
	void mutator_update(unsigned int marker_no) {}
	void retry_mutation() {}
	void retry_arg_building() {}

	void simulation_terminated() {}
    };


    struct ARGData {
	const core::ARG *arg;
	const core::Configuration *conf;
	const ProfileMonitor *monitor;
	ARGData(core::ARG *arg, core::Configuration *conf,
		ProfileMonitor *monitor)
	    : arg(arg), conf(conf), monitor(monitor)
	{};
	~ARGData()
	{
	    delete arg;
	    delete conf;
	    delete monitor;
	}
    };

}

#endif
