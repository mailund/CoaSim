/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
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

    void install_marker();
}

#endif // GUILE__MARKER_HH_INCLUDED
