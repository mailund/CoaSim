/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "simulate.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__ARG_PARAMETERS_HH_INCLUDED
# include "arg_parameters.hh"
#endif

using namespace guile;


#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif
#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include <Core/configuration.hh>
#endif
#ifndef CORE__SIMULATOR_HH_INCLUDED
# include <Core/simulator.hh>
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif


#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_H_INCLUDED
#endif
#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif


scm_t_bits guile::arg_tag;

namespace {
    struct ARGData {
	const core::ARG *arg;
	const core::Configuration *conf;
	ARGData(core::ARG *arg, core::Configuration *conf)
	    : arg(arg), conf(conf)
	{};
	~ARGData()
	{
	    delete arg;
	    delete conf;
	}
    };
}

static size_t
free_arg(SCM s_arg_data)
{
    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(s_arg_data);
    arg_data->~ARGData();
    scm_must_free(arg_data);
    return sizeof(ARGData);
}


static SCM
simulate(SCM arg_parameters_smob, SCM s_markers, SCM s_no_leaves)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_parameters_tag, 
				  arg_parameters_smob),
	       arg_parameters_smob, SCM_ARG1, "simulate");
    SCM_ASSERT(SCM_NFALSEP(scm_list_p(s_markers)),
	       s_markers, SCM_ARG2, "simulate");

    ARGParameters *p = (ARGParameters*) SCM_SMOB_DATA(arg_parameters_smob);

    std::vector<core::Marker*> markers;
    while (!SCM_NULLP(s_markers))
	{
	    SCM marker_smob = SCM_CAR(s_markers);
	    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::trait_marker_tag, marker_smob)
		       or
		       SCM_SMOB_PREDICATE(guile::snp_marker_tag, marker_smob)
		       or
		       SCM_SMOB_PREDICATE(guile::ms_marker_tag, marker_smob),
		       marker_smob, SCM_ARG2, "simulate");

	    core::Marker *marker = (core::Marker*) SCM_SMOB_DATA(marker_smob);
	    markers.push_back(marker);

	    s_markers = SCM_CDR(s_markers);
	}

    int no_leaves = scm_num2int(s_no_leaves, SCM_ARG3, "simulate");

    core::Configuration *conf 
	= new core::Configuration(no_leaves,
				  markers.begin(), markers.end(),
				  p->rho, p->Q, p->G, p->growth,
				  0); // FIXME: monitor?
    core::ARG *arg = core::Simulator::simulate(*conf);

    void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
    ARGData *arg_data = new(mem)ARGData(arg,conf);
    
    SCM_RETURN_NEWSMOB(guile::arg_tag, arg_data);
}

static SCM 
save_sequences(SCM arg_data_smob, SCM s_filename)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "save-sequences");
    SCM_ASSERT(SCM_NFALSEP(scm_string_p(s_filename)),
	       s_filename, SCM_ARG2, "save-sequences");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);
    const char *fname = SCM_STRING_CHARS(s_filename);

    std::ofstream os(fname);
    if (!os) scm_throw(scm_str2symbol("open-error"), s_filename);

    arg_data->arg->to_text(os);
    os.close();

    return SCM_EOL;
}

void
guile::install_simulate()
{
    guile::arg_tag = scm_make_smob_type("arg", sizeof(ARGData));
    scm_set_smob_free(guile::arg_tag, free_arg);

    scm_c_define_gsubr("simulate", 3, 0, 0, 
		       (scm_unused_struct*(*)())simulate);
    scm_c_define_gsubr("save-sequences", 2, 0, 0, 
		       (scm_unused_struct*(*)())save_sequences);
}


