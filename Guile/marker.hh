/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#ifndef GUILE__MARKER_HH_INCLUDED
#define GUILE__MARKER_HH_INCLUDED



#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {

    extern scm_t_bits trait_marker_tag;
    extern scm_t_bits snp_marker_tag;
    extern scm_t_bits ms_marker_tag;

    extern scm_t_bits scheme_marker_tag;

    inline bool marker_p(SCM smob)
    {
	return SCM_SMOB_PREDICATE(guile::trait_marker_tag, smob)
	    or
	    SCM_SMOB_PREDICATE(guile::snp_marker_tag, smob)
	    or
	    SCM_SMOB_PREDICATE(guile::ms_marker_tag, smob)
	    or
	    SCM_SMOB_PREDICATE(guile::scheme_marker_tag, smob);
    }


    inline void assert_marker(SCM smob, int arg_no, const char *caller)
    {
	SCM_ASSERT(marker_p(smob), smob, arg_no, caller);

    }

    void install_marker();
}

#endif // GUILE__MARKER_HH_INCLUDED
