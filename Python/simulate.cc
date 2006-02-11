/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__SIMULATE_HH_INCLUDED
# include "simulate.hh"
#endif
#ifndef PYTHON__MARKERS_HH_INCLUDED
# include "markers.hh"
#endif
#ifndef PYTHON__ARG_HH_INCLUDED
# include "arg.hh"
#endif

#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif
#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif
#ifndef CORE__EPOCHS_HH_INCLUDED
# include <Core/epochs.hh>
#endif
#ifndef CORE__SIMULATOR_HH_INCLUDED
# include <Core/simulator.hh>
#endif

PyObject *
simulate(PyObject *self, PyObject *args, PyObject *kwds)
{
    // FIXME: deal with population structure
    // FIXME: handle callbacks

    PyObject *py_markers;

    std::vector<int> sample_sizes;
    std::vector<core::Marker*> markers;
    std::vector<core::Event*> epochs;

    int no_samples;
    double rho;
    double Q;
    double gamma;
    double beta;

    char *kwd_list[] = {
	"n", "markers",
	"rho", "Q", "gamma", "beta", NULL
    };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "O!i|dddd", kwd_list,
				      &PyList_Type, &py_markers,
				      &no_samples,
				      &rho, &Q, &gamma, &beta))
        return 0; 		/* rethrow exception */

    for (int i = 0; i < PyList_Size(py_markers); ++i)
	{
	    PyObject *py_m = PyList_GetItem(py_markers, i);
	    if (!PyObject_TypeCheck(py_m, &MarkerType))
		{
		    PyErr_SetString(PyExc_TypeError, 
				    "arg #1 contains a non-marker");
		    return 0;
		}
	    markers.push_back(((MarkerObject*)py_m)->core_marker);
	}

    sample_sizes.push_back(no_samples);

    core::Configuration conf(sample_sizes.begin(), sample_sizes.end(),
			     markers.begin(), markers.end(),
			     epochs.begin(),  epochs.end(),
			     rho, Q, gamma, beta);
    core::ARG *arg = core::Simulator::simulate(conf);
    
    return wrap_arg(arg);
}


void
init_simulate(PyObject *module)
{
}
