/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "marker.hh"

scm_t_bits guile::trait_marker_tag;
scm_t_bits guile::snp_marker_tag;
scm_t_bits guile::ms_marker_tag;


static void * operator new(size_t, void*p) { return p; }

static size_t
free_trait_marker(SCM s_m)
{
    guile::TraitMarker *m = (guile::TraitMarker*) SCM_SMOB_DATA(s_m);
    m->guile::~TraitMarker();
    scm_must_free(m);
}

static int
print_trait_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    scm_puts("trait...", port);
    return 1;
}

static size_t
free_snp_marker(SCM s_m)
{
    guile::SNPMarker *m = (guile::SNPMarker*) SCM_SMOB_DATA(s_m);
    m->guile::~SNPMarker();
    scm_must_free(m);
}

static int
print_snp_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    scm_puts("snp...", port);
    return 1;
}

static size_t
free_ms_marker(SCM s_m)
{
    guile::MSMarker *m = (guile::MSMarker*) SCM_SMOB_DATA(s_m);
    m->guile::~MSMarker();
    scm_must_free(m);
}

static int
print_ms_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    scm_puts("ms...", port);
    return 1;
}

static SCM
trait_marker(SCM s_position, SCM s_low_freq, SCM s_high_freq)
{
    void *mem = scm_must_malloc(sizeof(guile::TraitMarker), "trait-marker");
    guile::TraitMarker *m = new(mem)guile::TraitMarker;
    SCM_RETURN_NEWSMOB(guile::trait_marker_tag, m);
}

static SCM
snp_marker(SCM s_position, SCM s_low_freq, SCM s_high_freq)
{
    void *mem = scm_must_malloc(sizeof(guile::SNPMarker), "snp-marker");
    guile::SNPMarker *m = new(mem)guile::SNPMarker;
    SCM_RETURN_NEWSMOB(guile::snp_marker_tag, m);
}

static SCM
ms_marker(SCM s_position, SCM s_alphabet_size)
{
    void *mem = scm_must_malloc(sizeof(guile::MSMarker), "ms-marker");
    guile::MSMarker *m = new(mem)guile::MSMarker;
    SCM_RETURN_NEWSMOB(guile::ms_marker_tag, m);
}

void
guile::install_marker()
{
    // create types
    guile::trait_marker_tag = scm_make_smob_type("trait-marker", 
						 sizeof(guile::TraitMarker));
    scm_set_smob_free( guile::trait_marker_tag,  free_trait_marker);
    scm_set_smob_print(guile::trait_marker_tag, print_trait_marker);

    guile::snp_marker_tag = scm_make_smob_type("snp-marker", 
					       sizeof(guile::SNPMarker));
    scm_set_smob_free( guile::snp_marker_tag,  free_snp_marker);
    scm_set_smob_print(guile::snp_marker_tag, print_snp_marker);

    guile::ms_marker_tag = scm_make_smob_type("ms-marker", 
					      sizeof(guile::MSMarker));
    scm_set_smob_free( guile::ms_marker_tag,  free_ms_marker);
    scm_set_smob_print(guile::ms_marker_tag, print_ms_marker);

    // install func for creating the types
    scm_c_define_gsubr("trait-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())trait_marker);
    scm_c_define_gsubr("snp-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())snp_marker);
    scm_c_define_gsubr("ms-marker", 2, 0, 0, 
		       (scm_unused_struct*(*)())ms_marker);

}
