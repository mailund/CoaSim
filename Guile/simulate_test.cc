/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */


#include "simulate.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__ARG_PARAMETERS_HH_INCLUDED
# include "arg_parameters.hh"
#endif

static void
real_main(void *dummy_closure, int argc, char *argv[])
{
    guile::install_marker();
    guile::install_arg_parameters();
    guile::install_simulate();
    scm_c_primitive_load("simulate-test.scm");
}

int
main(int argc, char *argv[])
{
    scm_boot_guile(argc, argv, real_main, NULL);
    return 0;
}
