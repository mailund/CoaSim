/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
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
    State state(*arg, i_conf.no_leaves());
    Population &population = state.population();

    // FIXME: these are no longer updated!!!
    unsigned long int no_iterations = 0;
    unsigned int coal_events = 0;
    unsigned int gene_conv_events = 0;
    unsigned int recomb_events = 0;

    double time = 0.0;

    Scheduler scheduler;
    if (i_conf.growth() > 0)
	scheduler.add_event(new CoalescenceEventGrowth(i_conf.growth()));
    else
	scheduler.add_event(new CoalescenceEvent);

    if (i_conf.rho() > 0)
	scheduler.add_event(new RecombinationEvent(i_conf.rho()));
    if (i_conf.gamma() > 0)
	scheduler.add_event(new GeneConversionEvent(i_conf.gamma(), 
						    i_conf.Q()));
    

    if (mon) mon->builder_update(i_conf.no_leaves(), // no nodes
				 i_conf.no_leaves(), // no "top" nodes
				 no_iterations, time,
				 coal_events, gene_conv_events, recomb_events);
			       

    while (population.size() > 1)
	{
	    Scheduler::time_event_t e = scheduler.next_event(state, time);
	    assert(e.second);
	    e.second->update_state(state, e.first, *arg, callbacks);
	    time = e.first;
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


