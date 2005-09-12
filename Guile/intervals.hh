/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef GUILE__INTERVALS_HH_INCLUDED
#define GUILE__INTERVALS_HH_INCLUDED


#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace core {
    class RetiredInterval;
}

namespace guile {

    void install_intervals();
    extern scm_t_bits interval_tag;
    extern scm_t_bits local_tree_tag;

    SCM wrap_interval(SCM arg, const core::RetiredInterval *rinterval);
}

#endif
