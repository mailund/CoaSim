/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef GUILE_MARKER_HH_INCLUDED
#define GUILE_MARKER_HH_INCLUDED

#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace guile {

    struct TraitMarker {
	double position;
	double low_freq;
	double high_freq;
    };
    struct SNPMarker {
	double position;
	double low_freq;
	double high_freq;
    };
    struct MSMarker {
	double position;
	int    alphabet_size;
    };

    extern scm_t_bits trait_marker_tag;
    extern scm_t_bits snp_marker_tag;
    extern scm_t_bits ms_marker_tag;

    void install_marker();
}

#endif // GUILE_MARKER_HH_INCLUDED
