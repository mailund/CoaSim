/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef GUILE__ARG_PARAMETERS_HH_INCLUDED
#define GUILE__ARG_PARAMETERS_HH_INCLUDED



#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {

    extern scm_t_bits arg_parameters_tag;

    struct ARGParameters {
	const double rho;
	const double Q;
	const double G;
	const double growth;
	ARGParameters(double rho, double Q, double G, double growth)
	    : rho(rho), Q(Q), G(G), growth(growth)
	{}
    };

    void install_arg_parameters();
}

#endif
