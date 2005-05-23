/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef CORE__SIMULATOR_HH_INCLUDED
#define CORE__SIMULATOR_HH_INCLUDED

namespace core {

    class ARG;
    class Configuration;
    class SimulationMonitor;
    class BuilderMonitor;

    namespace Simulator {
	// Returns the resulting ARG, or 0 if the simulation was aborted
	ARG *simulate(const Configuration &conf, 
		      SimulationMonitor *mon = 0,
		      BuilderMonitor *build_callbacks = 0,
		      bool keep_empty_intervals = false,
		      unsigned int random_seed = 0);
    }

}

#endif
