/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "builder.hh"

#ifndef CORE__DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef CORE__MONITOR_HH_INCLUDED
# include "monitor.hh"
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif
#ifndef CORE__BUILDER_EVENTS_HH_INCLUDED
# include "builder_events.hh"
#endif

using namespace core;


ARG * Builder::build(SimulationMonitor *mon,
		     BuilderMonitor    *callbacks,
		     bool keep_empty_intervals) const
{
    using namespace Distribution_functions;

    std::auto_ptr<ARG> arg(new ARG(i_conf, keep_empty_intervals));

    unsigned long int no_iterations = 0;
    unsigned int coal_events = 0;
    unsigned int gene_conv_events = 0;
    unsigned int recomb_events = 0;

    double time = 0.0;

    State state(*arg, callbacks, i_conf.no_leaves(), coal_events);
    Population &population = state.population();

    Scheduler scheduler;
    scheduler.add_event(population.coalescence_event());
    if (i_conf.growth() > 0)
	{
	    CoalescenceEventExtension *growth_coa_event
		= new CoalescenceEventGrowth(coal_events, i_conf.growth());
	    growth_coa_event->push(scheduler, state);
	}
	
    if (i_conf.rho() > 0)
	scheduler.add_event(new RecombinationEvent(recomb_events,
						   i_conf.rho()));
    if (i_conf.gamma() > 0)
	scheduler.add_event(new GeneConversionEvent(gene_conv_events,
						    i_conf.gamma(), 
						    i_conf.Q()));

    // FIXME: Validation of epochs!!!
    std::vector<Epoch*>::const_iterator i;
    for (i = i_conf.epochs_begin(); i != i_conf.epochs_end(); ++i)
	(*i)->add_events(scheduler, coal_events);

    if (mon) mon->builder_update(i_conf.no_leaves(), // no nodes
				 i_conf.no_leaves(), // no "top" nodes
				 no_iterations, time,
				 coal_events, gene_conv_events, recomb_events);
			       

    while (population.size() > 1)
	{
	    ++no_iterations;
	    Scheduler::time_event_t e = scheduler.next_event(state, time);
	    assert(e.second);
	    e.second->update_state(scheduler, state, e.first);
	    time = e.first;

	    if (mon and  (no_iterations % 50000)  == 0 )
		mon->builder_termination(arg->no_nodes(),
					 population.size(),
					 no_iterations, 
					 time,
					 coal_events, 
					 gene_conv_events,
					 recomb_events);
	}

    if (mon) mon->builder_termination(arg->no_nodes(),
				      population.size(),
				      no_iterations, time,
				      coal_events, gene_conv_events,
				      recomb_events);

    arg->sort_retired_intervals(); // NB! important, since the
				   // remaining functions rely on the
				   // retired intervals being sorted!
    return arg.release();
}


