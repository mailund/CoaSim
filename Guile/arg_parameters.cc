/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "arg_parameters.hh"

#ifndef NEW_INCLUDED
# include <new>
# define NEW_INCLUDED
#endif

using namespace guile;
scm_t_bits guile::arg_parameters_tag;


static size_t
free_arg_parameters(SCM s_p)
{
    ARGParameters *p = (ARGParameters*) SCM_SMOB_DATA(s_p);
    p->~ARGParameters();
    scm_must_free(p);
    return sizeof(ARGParameters);
}

static int
print_arg_parameters (SCM p_smob, SCM port, scm_print_state *pstate)
{
    ARGParameters *p = (ARGParameters*) SCM_SMOB_DATA(p_smob);
    scm_puts("(arg-parameters ", port);
    scm_display(scm_make_real(p->rho),    port); scm_puts(" ", port);
    scm_display(scm_make_real(p->Q),      port); scm_puts(" ", port);
    scm_display(scm_make_real(p->G),      port); scm_puts(" ", port);
    scm_display(scm_make_real(p->growth), port);
    scm_puts(")", port);
    return 1;
}

static SCM
arg_parameters(SCM s_rho, SCM s_G, SCM s_Q, SCM s_growth)
{
    double rho    = scm_num2dbl(s_rho,    "arg-parameters");
    double Q      = scm_num2dbl(s_Q,      "arg-parameters");
    double G      = scm_num2dbl(s_G,      "arg-parameters");
    double growth = scm_num2dbl(s_growth, "arg-parameters");
    void *mem = scm_must_malloc(sizeof(ARGParameters), "arg-parameters");
    ARGParameters *p = new(mem)ARGParameters(rho, Q, G, growth);
    SCM_RETURN_NEWSMOB(arg_parameters_tag, p);
}

void
guile::install_arg_parameters()
{
    // create types
    guile::arg_parameters_tag = scm_make_smob_type("arg-parameters", 
						   sizeof(ARGParameters));
    scm_set_smob_free( guile::arg_parameters_tag,  free_arg_parameters);
    scm_set_smob_print(guile::arg_parameters_tag, print_arg_parameters);

    // install func for creating the types
    scm_c_define_gsubr("arg-parameters", 4, 0, 0, 
		       (scm_unused_struct*(*)())arg_parameters);
}
