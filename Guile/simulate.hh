/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef GUILE__SIMULATE_HH_INCLUDED
#define GUILE__SIMULATE_HH_INCLUDED


#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {
    void install_simulate();
    extern scm_t_bits arg_tag;

}

#endif
