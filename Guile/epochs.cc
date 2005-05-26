/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "epochs.hh"

#ifndef GUILE__EXCEPTIONS_HH_INCLUDED
# include "exceptions.hh"
#endif

#ifndef CORE_EPOCHS_HH_INCLUDED
# include <Core/epochs.hh>
#endif

#ifndef NEW_INCLUDED
# include <new>
# define NEW_INCLUDED
#endif

scm_t_bits guile::bottleneck_epoch_tag;

using namespace guile;

#include <iostream> // FIXME: REMOVE AFTER DEBUGGING

static size_t
free_bottleneck_epoch(SCM smob)
{
    core::BottleNeck *bn = (core::BottleNeck*) SCM_SMOB_DATA(smob);
    bn->~BottleNeck();
    scm_must_free(bn);
    return sizeof(core::BottleNeck);
}

static int
print_bottleneck_epoch (SCM smob, SCM port, scm_print_state *pstate)
{
    core::BottleNeck *bn = (core::BottleNeck*) SCM_SMOB_DATA(smob);
    scm_puts("(bottleneck ", port);
    scm_display(scm_make_real(bn->scale_fraction()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(bn->start_point()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(bn->end_point()), port);
    scm_puts(")", port);
    return 1;
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="bottleneck">
  <brief>Makes a bottleneck epoch.</brief>
  <prototype>(bottleneck scale-fraction begin-time end-time)</prototype>
  <example>(bottleneck 0.1 0.9 1.22)</example>
  <description>
    <p>
       Specify a bottleneck the simulation process will pass through.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
bottleneck(SCM s_scale_fraction, SCM s_start_time, SCM s_end_time)
{
    double scale_fraction = scm_num2dbl(s_scale_fraction, "bottleneck");
    double start_time     = scm_num2dbl(s_start_time,     "bottleneck");
    double end_time       = scm_num2dbl(s_end_time,       "bottleneck");

    if (scale_fraction < 0)
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);
    if (not (start_time < end_time))
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);

    void *mem = scm_must_malloc(sizeof(core::BottleNeck), "bottleneck");
    assert(mem);
    core::BottleNeck *bn = new(mem)core::BottleNeck(scale_fraction,
						    start_time, end_time);
    SCM_RETURN_NEWSMOB(guile::bottleneck_epoch_tag, bn);
}


void
guile::install_epochs()
{

    // create types
    guile::bottleneck_epoch_tag = scm_make_smob_type("bottleneck", 
						     sizeof(core::BottleNeck));
    scm_set_smob_free( guile::bottleneck_epoch_tag,  free_bottleneck_epoch);
    scm_set_smob_print(guile::bottleneck_epoch_tag, print_bottleneck_epoch);


    // install func for creating the types
    scm_c_define_gsubr("bottleneck", 3, 0, 0, 
		       (scm_unused_struct*(*)())bottleneck);
}
