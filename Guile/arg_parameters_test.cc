/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */


#include "arg_parameters.hh"

#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif

static void
real_main(void *dummy_closure, int argc, char *argv[])
{
    guile::install_arg_parameters();
    scm_c_primitive_load("arg-parameters-test.scm");
}

int
main(int argc, char *argv[])
{
    scm_boot_guile(argc, argv, real_main, NULL);
    return 0;
}
