/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef SIMULATOR_HH_INCLUDED
#define SIMULATOR_HH_INCLUDED

class ARG;
class Configuration;

namespace Simulator {
    // Returns the resulting ARG, or 0 if the simulation was aborted
    ARG *simulate(const Configuration &conf);
}

#endif // SIMULATOR_HH_INCLUDED
