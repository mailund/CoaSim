/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "simulator.hh"

#ifndef CORE__BUILDER_HH_INCLUDED
# include "builder.hh"
#endif
#ifndef CORE__DESCENDER_HH_INCLUDED
# include "descender.hh"
#endif
#ifndef CORE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef CORE__MONITOR_HH_INCLUDED
# include "monitor.hh"
#endif
#ifndef SYS_TIME_H_INCLUDED
# include <sys/time.h>
# define SYS_TIME_H_INCLUDED
#endif

using namespace core;

ARG *
core::Simulator::simulate(const Configuration &conf,
			  SimulationMonitor *mon,
			  BuilderMonitor *build_callbacks,
			  unsigned int random_seed)
{
    Builder builder(conf);
    Descender descender(conf);
    ARG *arg = 0;

    // set rand seed
    if (!random_seed)
	{
	    // use time if no seed is explicitly given
	    struct timeval tv; struct timezone tz;
	    gettimeofday(&tv,&tz);
	    random_seed = tv.tv_usec;
	}
    std::srand(random_seed);


    try {

    retry:
	try {
	    if (mon) mon->start_arg_building(conf.no_leaves());
	    arg = builder.build(mon, build_callbacks);
	    if (mon) mon->start_mutating();
	    descender.evolve(*arg, mon);
	} catch (Mutator::retry_arg&) {
	    if (mon) mon->retry_arg_building();
	    delete arg; arg = 0;
	    goto retry;
	}

	if (mon) mon->simulation_terminated();

    } catch(SimulationMonitor::AbortSimulation&) {
	if (arg) delete arg; arg = 0;
    }

    return arg;
}
