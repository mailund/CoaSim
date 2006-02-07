/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */


#include "epochs.hh"

#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif

static void
real_main(void *dummy_closure, int argc, char *argv[])
{
    guile::install_epochs();
    scm_c_primitive_load("epochs-test.scm");
}

int
main(int argc, char *argv[])
{
    scm_boot_guile(argc, argv, real_main, NULL);
    return 0;
}
