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
scm_t_bits guile::growth_epoch_tag;
scm_t_bits guile::migration_epoch_tag;
scm_t_bits guile::population_merge_epoch_tag;

using namespace guile;

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
    scm_display(scm_long2num(bn->population()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(bn->scale_fraction()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(bn->start_point()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(bn->end_point()), port);
    scm_puts(")", port);
    return 1;
}

static size_t
free_growth_epoch(SCM smob)
{
    core::Growth *g = (core::Growth*) SCM_SMOB_DATA(smob);
    g->~Growth();
    scm_must_free(g);
    return sizeof(core::Growth);
}

static int
print_growth_epoch (SCM smob, SCM port, scm_print_state *pstate)
{
    core::Growth *g = (core::Growth*) SCM_SMOB_DATA(smob);
    scm_puts("(growth ", port);
    scm_display(scm_long2num(g->population()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(g->beta()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(g->start_point()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(g->end_point()), port);
    scm_puts(")", port);
    return 1;
}

static size_t
free_migration_epoch(SCM smob)
{
    core::Migration *m = (core::Migration*) SCM_SMOB_DATA(smob);
    m->~Migration();
    scm_must_free(m);
    return sizeof(core::Migration);
}

static int
print_migration_epoch (SCM smob, SCM port, scm_print_state *pstate)
{
    core::Migration *m = (core::Migration*) SCM_SMOB_DATA(smob);
    scm_puts("(migration ", port);
    scm_display(scm_long2num(m->source()), port); 
    scm_puts(" ", port);
    scm_display(scm_long2num(m->destination()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(m->migration_rate()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(m->start_time()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(m->end_time()), port);
    scm_puts(")", port);
    return 1;
}


static size_t
free_population_merge_epoch(SCM smob)
{
    core::PopulationMerge *pm = (core::PopulationMerge*) SCM_SMOB_DATA(smob);
    pm->~PopulationMerge();
    scm_must_free(pm);
    return sizeof(core::PopulationMerge);
}

static int
print_population_merge_epoch (SCM smob, SCM port, scm_print_state *pstate)
{
    core::PopulationMerge *pm = (core::PopulationMerge*) SCM_SMOB_DATA(smob);
    scm_puts("(population-merge ", port);
    scm_display(scm_long2num(pm->population1()), port); 
    scm_puts(" ", port);
    scm_display(scm_long2num(pm->population2()), port); 
    scm_puts(" ", port);
    scm_display(scm_make_real(pm->merge_time()), port);
    scm_puts(")", port);
    return 1;
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="bottleneck">
  <brief>Makes a bottleneck epoch.</brief>
  <prototype>(bottleneck population scale-fraction begin-time end-time)</prototype>
  <example>(bottleneck 0 0.1 0.9 1.22)</example>
  <description>
    <p>
       Specify a bottleneck the simulation process will pass through.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
bottleneck(SCM s_pop, SCM s_scale_fraction, SCM s_start_time, SCM s_end_time)
{
    int    pop            = scm_num2int(s_pop, 1,         "bottleneck");
    double scale_fraction = scm_num2dbl(s_scale_fraction, "bottleneck");
    double start_time     = scm_num2dbl(s_start_time,     "bottleneck");
    double end_time       = scm_num2dbl(s_end_time,       "bottleneck");

    if (scale_fraction < 0)
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);
    if (not (start_time < end_time))
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);

    void *mem = scm_must_malloc(sizeof(core::BottleNeck), "bottleneck");
    assert(mem);
    core::BottleNeck *bn =
	new(mem)core::BottleNeck(pop, scale_fraction, start_time, end_time);
    SCM_RETURN_NEWSMOB(guile::bottleneck_epoch_tag, bn);
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="growth">
  <brief>Makes a growth epoch.</brief>
  <prototype>(growth population beta begin-time end-time)</prototype>
  <example>(growth 0 10 0.9 1.22)</example>
  <description>
    <p>
       Specify a period of exponetial growth.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
growth(SCM s_pop, SCM s_beta, SCM s_start_time, SCM s_end_time)
{
    int    pop        = scm_num2int(s_pop, 1,     "growth");
    double beta       = scm_num2dbl(s_beta,       "growth");
    double start_time = scm_num2dbl(s_start_time, "growth");
    double end_time   = scm_num2dbl(s_end_time,   "growth");

    if (beta < 0)
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);
    if (not (start_time < end_time))
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);

    void *mem = scm_must_malloc(sizeof(core::Growth), "growth");
    assert(mem);
    core::Growth *g = new(mem)core::Growth(pop, beta, start_time, end_time);
    SCM_RETURN_NEWSMOB(guile::growth_epoch_tag, g);
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="migration">
  <brief>Makes a migration epoch.</brief>
  <prototype>(migration source-population destination-population rate begin-time end-time)</prototype>
  <example>(migration 0 1 0.1 0.9 1.22)</example>
  <description>
    <p>
       Specify a period of migration from population `source-population'
       to `destination-population' with scaled migration rate `rate'.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
migration(SCM s_src, SCM s_dst, SCM s_rate, SCM s_start_time, SCM s_end_time)
{
    int    src        = scm_num2int(s_src, 1,     "migration");
    int    dst        = scm_num2int(s_dst, 2,     "migration");
    double rate       = scm_num2dbl(s_rate,       "migration");
    double start_time = scm_num2dbl(s_start_time, "migration");
    double end_time   = scm_num2dbl(s_end_time,   "migration");

    if (rate < 0)
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);
    if (not (start_time < end_time))
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);

    void *mem = scm_must_malloc(sizeof(core::Migration), "migration");
    assert(mem);
    core::Migration *g = 
	new(mem)core::Migration(src, dst, rate, start_time, end_time);

    SCM_RETURN_NEWSMOB(guile::migration_epoch_tag, g);
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="population-merge">
  <brief>Merges two sub-populations.</brief>
  <prototype>(population-merge pop1 pop2 merge-time)</prototype>
  <example>(population-merge 0 1 0.9)</example>
  <description>
    <p>
       Sets up the merge time for two sub-populations.  The effect
       of the merge is that all individuals in population 2 is moved
       to population 1.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
population_merge(SCM s_pop1, SCM s_pop2, SCM s_merge_time)
{
    int pop1          = scm_num2int(s_pop1,1,     "population-merge");
    int pop2          = scm_num2int(s_pop2,2,     "population-merge");
    double merge_time = scm_num2dbl(s_merge_time, "population-merge");

    if (merge_time < 0)
	scm_throw(scm_str2symbol("illegal-epoch"), SCM_EOL);

    void *mem = scm_must_malloc(sizeof(core::PopulationMerge),
				"population-merge");
    assert(mem);
    core::PopulationMerge *pm = new(mem)core::PopulationMerge(pop1, pop2,
							      merge_time);
    SCM_RETURN_NEWSMOB(guile::population_merge_epoch_tag, pm);
}


void
guile::install_epochs()
{
    // create types
    guile::bottleneck_epoch_tag =
	scm_make_smob_type("bottleneck", sizeof(core::BottleNeck));
    scm_set_smob_free( guile::bottleneck_epoch_tag,  free_bottleneck_epoch);
    scm_set_smob_print(guile::bottleneck_epoch_tag, print_bottleneck_epoch);

    guile::growth_epoch_tag =
	scm_make_smob_type("growth", sizeof(core::Growth));
    scm_set_smob_free( guile::growth_epoch_tag,  free_growth_epoch);
    scm_set_smob_print(guile::growth_epoch_tag, print_growth_epoch);

    guile::migration_epoch_tag =
	scm_make_smob_type("migration", sizeof(core::Migration));
    scm_set_smob_free( guile::migration_epoch_tag,  free_migration_epoch);
    scm_set_smob_print(guile::migration_epoch_tag, print_migration_epoch);

    guile::population_merge_epoch_tag =
	scm_make_smob_type("population-merge", sizeof(core::PopulationMerge));
    scm_set_smob_free(guile::population_merge_epoch_tag,  
		      free_population_merge_epoch);
    scm_set_smob_print(guile::population_merge_epoch_tag, 
		       print_population_merge_epoch);


    // install funcs for creating the types
    scm_c_define_gsubr("bottleneck", 4, 0, 0, 
		       (scm_unused_struct*(*)())bottleneck);
    scm_c_define_gsubr("growth", 4, 0, 0, 
		       (scm_unused_struct*(*)())growth);
    scm_c_define_gsubr("migration", 5, 0, 0, 
		       (scm_unused_struct*(*)())migration);
    scm_c_define_gsubr("population-merge", 3, 0, 0, 
		       (scm_unused_struct*(*)())population_merge);
}
