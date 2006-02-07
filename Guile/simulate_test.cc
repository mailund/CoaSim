/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */


#include "simulate.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__EPOCHS_HH_INCLUDED
# include "epochs.hh"
#endif
#ifndef GUILE__NODES_HH_INCLUDED
# include "nodes.hh"
#endif
#ifndef GUILE__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif

static void
real_main(void *dummy_closure, int argc, char *argv[])
{
    // let use use :keyword instead of #:keyword
    scm_c_eval_string("(read-set! keywords 'prefix)");

    guile::install_marker();
    guile::install_epochs();
    guile::install_nodes();
    guile::install_intervals();
    guile::install_simulate();
    scm_c_primitive_load("simulate-test.scm");
}

int
main(int argc, char *argv[])
{
    scm_boot_guile(argc, argv, real_main, NULL);
    return 0;
}
