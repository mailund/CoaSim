/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "marker.hh"

#ifndef GUILE__EXCEPTIONS_HH_INCLUDED
# include "exceptions.hh"
#endif
#ifndef GUILE__NODES_HH_INCLUDED
# include "nodes.hh"
#endif

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

scm_t_bits guile::scheme_marker_tag;

using namespace guile;

static size_t
free_trait_marker(SCM s_m)
{
    core::TraitMarker *m = (core::TraitMarker*) SCM_SMOB_DATA(s_m);
    m->~TraitMarker();
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
    m->~SNPMarker();
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
    m->~MicroSatelliteMarker();
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
    scm_display(scm_make_real(m->theta()),     port); scm_puts(" ", port);
    scm_display(scm_long2num(m->K()),          port);
    scm_puts(")", port);

    return 1;
}



namespace {
    using namespace core;
    struct SchemeMarker : public core::Marker {

	SCM default_value_cb;
	SCM mutate_cb;

	// polymorphic copying
	virtual Marker *copy() const;

	virtual bool run_first() const;
	virtual int default_value() const;

	// creates a new mutator -- the mutator must be deleted after use.
	virtual Mutator *create_mutator(const Configuration &conf,
					const RetiredInterval &ri) const;

	virtual const char * type() const;

    public:

	SchemeMarker(double position, SCM default_value, SCM mutate) 
	    : core::Marker(position),
	      default_value_cb(default_value),
	      mutate_cb(mutate)
	{}	

    };

    class SchemeMutator : public core::Mutator {
	const SchemeMarker i_marker;

	int mutate(const Node &parent, const Node &child, int parent_allele);

    public:
	SchemeMutator(const SchemeMarker &marker) : i_marker(marker) {}
    };

    Marker *SchemeMarker::copy() const
    {
	return new SchemeMarker(*this);
    }

    bool SchemeMarker::run_first() const
    {
	return false;  // FIXME
    }

    
    const char *SchemeMarker::type() const 
    {
	return "scheme-marker"; 
    }

    int SchemeMarker::default_value() const
    {
	SCM val = wrapped_apply(default_value_cb, SCM_EOL);
	// FIXME: exceptions here???
	return scm_num2int(val, 1, "*default-value*");
    }

    Mutator *SchemeMarker::create_mutator(const Configuration &conf,
					  const RetiredInterval &ri) const
    {
	return new SchemeMutator(*this);
    }

    int SchemeMutator::mutate(const Node &parent, const Node &child, 
			      int parent_allele)
    {
	SCM args = scm_list_3(wrap_node(SCM_EOL, &parent),
			      wrap_node(SCM_EOL, &child),
			      scm_long2num(parent_allele));
	SCM val = wrapped_apply(i_marker.mutate_cb, args);

	// FIXME: exceptions here???
	return scm_num2int(val, 1, "*mutate*");
    }

}

static SCM 
mark_scheme_marker (SCM s_m)
{
    SchemeMarker *marker = (SchemeMarker*) SCM_SMOB_DATA(s_m);
    scm_gc_mark(marker->default_value_cb);
    return SCM_BOOL_F;
}

static size_t
free_scheme_marker(SCM s_m)
{
    SchemeMarker *m = (SchemeMarker*) SCM_SMOB_DATA(s_m);
    m->~SchemeMarker();
    scm_must_free(m);
    return sizeof(SchemeMarker);
}

static int
print_scheme_marker (SCM marker_smob, SCM port, scm_print_state *pstate)
{
    SchemeMarker *m = (SchemeMarker*) SCM_SMOB_DATA(marker_smob);
    scm_puts("*user-define-marker*", port);
    return 1;
}






/* --<GUILE COMMENT>---------------------------------------------

<method name="trait-marker">
  <brief>Creates a trait marker on the simulated region.</brief>
  <prototype>(trait-marker position low-freq high-freq)</prototype>
  <example>(trait-marker 0.5 0.18 0.22)</example>
  <description>
    <p>
      Creates a trait marker on the simulated region.  The first
      parameter determines the position (between 0 and 1) along the
      region, the second the lowest frequency accepted for the
      mutation type, and the third the highest frequency accepted for the
      mutation type.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
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


/* --<GUILE COMMENT>---------------------------------------------

<method name="snp-marker">
  <brief>Creates a SNP marker on the simulated region.</brief>
  <prototype>(snp-marker position low-freq high-freq)</prototype>
  <example>(snp-marker 0.5 0.1 0.9)</example>
  <description>
    <p>
      Creates a SNP marker on the simulated region.  The first
      parameter determines the position (between 0 and 1) along the
      region, the second the lowest frequency accepted for the
      mutation type, and the third the highest frequency accepted for the
      mutation type.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
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

/* --<GUILE COMMENT>---------------------------------------------

<method name="ms-marker">
  <brief>Creates a micro-satellite marker on the simulated region.</brief>
  <prototype>(ms-marker position theta K)</prototype>
  <example>(ms-marker 0.5 0.2 10)</example>
  <description>
    <p>
      Creates a micro-satellite marker on the simulated region.  The
      first parameter determines the position (between 0 and 1) along
      the region, the second the mutation rate of the marker, and the
      third the size of the alphabet for the marker (in the K allele
      model).
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
ms_marker(SCM s_position, SCM s_theta, SCM s_alphabet_size)
{
    double position      = scm_num2dbl(s_position, "ms-marker");
    double theta         = scm_num2dbl(s_theta,    "ms-marker");
    int    alphabet_size = scm_num2int(s_alphabet_size, SCM_ARG3, "ms-marker");
    void *mem = scm_must_malloc(sizeof(core::MicroSatelliteMarker), 
				"ms-marker");
    core::MicroSatelliteMarker *m 
	= new(mem)core::MicroSatelliteMarker(position, theta, alphabet_size);
    SCM_RETURN_NEWSMOB(guile::ms_marker_tag, m);
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="trait-marker?">
  <brief>A predicate that evalutes to true for trait markers only.</brief>
  <prototype>(trait-marker? marker)</prototype>
  <example>(trait-marker? marker)</example>
  <description>
    <p>
     A predicate that evalutes to true for trait markers and false for
     all other types.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
trait_marker_p(SCM marker_smob)
{
    return SCM_BOOL(SCM_SMOB_PREDICATE(guile::trait_marker_tag, marker_smob));
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="snp-marker?">
  <brief>A predicate that evalutes to true for SNP markers only.</brief>
  <prototype>(snp-marker? marker)</prototype>
  <example>(snp-marker? marker)</example>
  <description>
    <p>
     A predicate that evalutes to true for SNP markers and false for
     all other types.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
snp_marker_p(SCM marker_smob)
{
    return SCM_BOOL(SCM_SMOB_PREDICATE(guile::snp_marker_tag, marker_smob));
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="ms-marker?">
  <brief>A predicate that evalutes to true for micro-satellite markers only.</brief>
  <prototype>(ms-marker? marker)</prototype>
  <example>(ms-marker? marker)</example>
  <description>
    <p>
     A predicate that evalutes to true for micro-satellite  markers and false
     for all other types.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
ms_marker_p(SCM marker_smob)
{
    return SCM_BOOL(SCM_SMOB_PREDICATE(guile::ms_marker_tag, marker_smob));
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="position">
  <brief>Returns the position of a marker.</brief>
  <prototype>(position marker)</prototype>
  <example>(position marker)</example>
  <description>
    <p>Returns the position of a marker.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
position(SCM marker_smob)
{
    assert_marker(marker_smob, SCM_ARG1, "position");
    core::Marker *marker = (core::Marker*) SCM_SMOB_DATA(marker_smob);
    return scm_make_real(marker->position());
}



static SCM
custom_marker(SCM s_position, SCM default_value, SCM mutate)
{
    double position  = scm_num2dbl(s_position,  "c-custom-marker");

    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(default_value)),
	       default_value, SCM_ARG2, "c-custom-marker");
    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(mutate)),
	       mutate, SCM_ARG3, "c-custom-marker");

    void *mem = scm_must_malloc(sizeof(SchemeMarker), "c-custom-marker");
    SchemeMarker *m = new(mem)SchemeMarker(position, default_value, mutate);
    SCM_RETURN_NEWSMOB(guile::scheme_marker_tag, m);
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

    guile::scheme_marker_tag = scm_make_smob_type("*user-defined-marker*", 
						  sizeof(SchemeMarker));
    scm_set_smob_mark( guile::scheme_marker_tag, mark_scheme_marker);
    scm_set_smob_free( guile::scheme_marker_tag, free_scheme_marker);
    scm_set_smob_print(guile::scheme_marker_tag, print_scheme_marker);


    // install func for creating the types
    scm_c_define_gsubr("trait-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())trait_marker);
    scm_c_define_gsubr("snp-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())snp_marker);
    scm_c_define_gsubr("ms-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())ms_marker);

    scm_c_define_gsubr("c-custom-marker", 3, 0, 0, 
		       (scm_unused_struct*(*)())custom_marker);
    scm_c_eval_string("(define (custom-marker pos default mutate)"
		      "  (let ((default-func (if (procedure? default) "
		      "                          default"
		      "                          (lambda () default))))"
		      "     (c-custom-marker pos default-func mutate)))");



    // other functions
    scm_c_define_gsubr("trait-marker?", 1, 0, 0, 
		       (scm_unused_struct*(*)())trait_marker_p);
    scm_c_define_gsubr("snp-marker?", 1, 0, 0, 
		       (scm_unused_struct*(*)())snp_marker_p);
    scm_c_define_gsubr("ms-marker?", 1, 0, 0, 
		       (scm_unused_struct*(*)())ms_marker_p);

    scm_c_define_gsubr("position", 1, 0, 0, (scm_unused_struct*(*)())position);

}
