/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#ifndef GUILE__EPOCHS_HH_INCLUDED
#define GUILE__EPOCHS_HH_INCLUDED



#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {

    extern scm_t_bits bottleneck_epoch_tag;
    extern scm_t_bits growth_epoch_tag;
    extern scm_t_bits migration_epoch_tag;
    extern scm_t_bits population_merge_epoch_tag;
    void install_epochs();

    inline bool epoch_p(SCM smob)
    {
	return SCM_SMOB_PREDICATE(bottleneck_epoch_tag, smob)
	    or SCM_SMOB_PREDICATE(growth_epoch_tag, smob)
	    or SCM_SMOB_PREDICATE(migration_epoch_tag, smob)
	    or SCM_SMOB_PREDICATE(population_merge_epoch_tag, smob)
	    ;
    }

    inline void assert_epoch(SCM smob, int arg_no, const char *caller)
    {
	SCM_ASSERT(epoch_p(smob), smob, arg_no, caller);

    }

}

#endif // GUILE__EPOCHS_HH_INCLUDED
