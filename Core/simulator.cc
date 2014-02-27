/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#include "simulator.hh"

#include "builder.hh"
#include "descender.hh"
#include "marker.hh"
#include "monitor.hh"
#include "node.hh"

#include <sys/time.h>
#include <memory>

using namespace core;

ARG *
core::Simulator::simulate(const Configuration &conf,
                          BuilderMonitor *build_callbacks,
                          bool keep_empty_intervals,
                          bool keep_migration_events,
                          unsigned int random_seed)
{
    Builder builder(conf);
    Descender descender(conf);
    
    // set rand seed
    if (!random_seed)
	{
	    // use time if no seed is explicitly given
	    struct timeval tv; struct timezone tz;
	    gettimeofday(&tv,&tz);
	    random_seed = tv.tv_usec;
	}
    std::srand(random_seed);
    
    
    ARG *arg = 0;
retry:
    try {
        std::auto_ptr<ARG> memsafe(builder.build(build_callbacks, 
                                                 keep_empty_intervals,
                                                 keep_migration_events));
        descender.evolve(*memsafe.get());
        arg = memsafe.release();
    } catch (Mutator::retry_arg&) {
        goto retry;
    } catch(SimulationMonitor::AbortSimulation&) {
        return 0;
    }
    
    return arg;
}
