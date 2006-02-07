/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
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
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif

#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {
    void install_simulate();
    extern scm_t_bits arg_tag;


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

#endif
