/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "marker.hh"

#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif

#ifndef NEW_INCLUDED
# include <new>
# define NEW_INCLUDED
#endif

scm_t_bits guile::trait_marker_tag;
scm_t_bits guile::snp_marker_tag;
scm_t_bits guile::ms_marker_tag;



static size_t
free_trait_marker(SCM s_m)
{
    core::TraitMarker *m = (core::TraitMarker*) SCM_SMOB_DATA(s_m);
    m->core::~TraitMarker();
    scm_must_free(m);
    return sizeof(core::TraitMarker);
}

static int
print_trait_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    core::TraitMarker *m = (core::TraitMarker*) SCM_SMOB_DATA(marker_smob);
    scm_puts("(trait ", port);
    scm_display(scm_make_real(m->position()),  port); scm_puts(" ", port);
    scm_display(scm_make_real(m->low_freq()),  port); scm_puts(" ", port);
    scm_display(scm_make_real(m->high_freq()), port);
    scm_puts(")", port);
    return 1;
}

static size_t
free_snp_marker(SCM s_m)
{
    core::SNPMarker *m = (core::SNPMarker*) SCM_SMOB_DATA(s_m);
    m->core::~SNPMarker();
    scm_must_free(m);
    return sizeof(core::SNPMarker);
}

static int
print_snp_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    core::SNPMarker *m = (core::SNPMarker*) SCM_SMOB_DATA(marker_smob);
    scm_puts("(snp-marker ", port);
    scm_display(scm_make_real(m->position()),  port); scm_puts(" ", port);
    scm_display(scm_make_real(m->low_freq()),  port); scm_puts(" ", port);
    scm_display(scm_make_real(m->high_freq()), port);
    scm_puts(")", port);
    return 1;
}

static size_t
free_ms_marker(SCM s_m)
{
    core::MicroSatelliteMarker *m 
	= (core::MicroSatelliteMarker*) SCM_SMOB_DATA(s_m);
    m->core::~MicroSatelliteMarker();
    scm_must_free(m);
    return sizeof(core::MicroSatelliteMarker);
}

static int
print_ms_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    core::MicroSatelliteMarker *m = 
	(core::MicroSatelliteMarker*) SCM_SMOB_DATA(marker_smob);
    scm_puts("(ms-marker ", port);
    scm_display(scm_make_real(m->position()),  port); scm_puts(" ", port);
    scm_display(scm_make_real(m->mu()),        port); scm_puts(" ", port);
    scm_display(scm_long2num(m->size()),       port);
    scm_puts(")", port);

    return 1;
}

static SCM
trait_marker(SCM s_position, SCM s_low_freq, SCM s_high_freq)
{
    double position  = scm_num2dbl(s_position,  "trait-marker");
    double low_freq  = scm_num2dbl(s_low_freq,  "trait-marker");
    double high_freq = scm_num2dbl(s_high_freq, "trait-marker");
    void *mem = scm_must_malloc(sizeof(core::TraitMarker), "trait-marker");
    core::TraitMarker *m = new(mem)core::TraitMarker(position, 
						     low_freq, high_freq);
    SCM_RETURN_NEWSMOB(guile::trait_marker_tag, m);
}

static SCM
snp_marker(SCM s_position, SCM s_low_freq, SCM s_high_freq)
{
    double position  = scm_num2dbl(s_position,  "snp-marker");
    double low_freq  = scm_num2dbl(s_low_freq,  "snp-marker");
    double high_freq = scm_num2dbl(s_high_freq, "snp-marker");
    void *mem = scm_must_malloc(sizeof(core::SNPMarker), "snp-marker");
    core::SNPMarker *m = new(mem)core::SNPMarker(position,
						 low_freq, high_freq);
    SCM_RETURN_NEWSMOB(guile::snp_marker_tag, m);
}

static SCM
ms_marker(SCM s_position, SCM s_mu, SCM s_alphabet_size)
{
    double position      = scm_num2dbl(s_position, "ms-marker");
    double mu            = scm_num2dbl(s_mu,       "ms-marker");
    int    alphabet_size = scm_num2int(s_alphabet_size, SCM_ARG3, "ms-marker");
    void *mem = scm_must_malloc(sizeof(core::MicroSatelliteMarker), 
				"ms-marker");
    core::MicroSatelliteMarker *m 
	= new(mem)core::MicroSatelliteMarker(position, mu);
    for (int i = 0; i < alphabet_size; ++i) m->add_value(i);
    SCM_RETURN_NEWSMOB(guile::ms_marker_tag, m);
}

void
guile::install_marker()
{
    // create types
    guile::trait_marker_tag = scm_make_smob_type("trait-marker", 
						 sizeof(core::TraitMarker));
    scm_set_smob_free( guile::trait_marker_tag,  free_trait_marker);
    scm_set_smob_print(guile::trait_marker_tag, print_trait_marker);

    guile::snp_marker_tag = scm_make_smob_type("snp-marker", 
					       sizeof(core::SNPMarker));
    scm_set_smob_free( guile::snp_marker_tag,  free_snp_marker);
    scm_set_smob_print(guile::snp_marker_tag, print_snp_marker);

    guile::ms_marker_tag = scm_make_smob_type("ms-marker", 
					      sizeof(core::MicroSatelliteMarker));
    scm_set_smob_free( guile::ms_marker_tag,  free_ms_marker);
    scm_set_smob_print(guile::ms_marker_tag, print_ms_marker);

    // install func for creating the types
    scm_c_define_gsubr("trait-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())trait_marker);
    scm_c_define_gsubr("snp-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())snp_marker);
    scm_c_define_gsubr("ms-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())ms_marker);

}
