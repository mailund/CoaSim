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

    int no_samples = 0;

    double rho = 0;
    double Q = 0;
    double gamma = 0;
    double beta = 0;

    PyObject *py_keep_empty = Py_False;
    long seed = 0;

    char *kwd_list[] = {
	"n", "markers",
	"rho", "Q", "gamma", "beta", 
	"keepEmptyIntervals",
	"seed",
	NULL
    };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, 
				      "O!i|ddddO!l", kwd_list,
				      &PyList_Type, &py_markers,
				      &no_samples,
				      &rho, &Q, &gamma, &beta,
				      &PyBool_Type, &py_keep_empty,
				      &seed))
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
	    core::Marker *core_marker = ((MarkerObject*)py_m)->core_marker;
	    if (!core_marker)
		{
		    PyErr_SetString(PyExc_ValueError, 
				    "arg #1 contains an un-initialized marker");
		    return 0;
		}
	    markers.push_back(core_marker);
	}

    sample_sizes.push_back(no_samples);
    core::ARG *arg = 0;


    try {
	core::Configuration conf(sample_sizes.begin(), sample_sizes.end(),
				 markers.begin(), markers.end(),
				 epochs.begin(),  epochs.end(),
				 rho, Q, gamma, beta);
	//                                callbacks
	arg = core::Simulator::simulate(conf, 0, py_keep_empty==Py_True, seed);

    } catch(core::Configuration::out_of_sequence&) {
	PyErr_SetString(PyExc_ValueError, "Marker positions out of sequence.");
	return 0;
    } catch(core::Configuration::non_pos_pop_size&) {
	PyErr_SetString(PyExc_ValueError, "Non-positive sample size.");
	return 0;
    } catch(core::Configuration::negative_rate &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return 0;
    } catch(PyException &pex) {
	// propagate exception
	return 0;
    } catch(std::exception &ex) {
	PyErr_SetString(PyExc_RuntimeError, ex.what());
	return 0;
    }
    
    return wrap_arg(arg);
}


void
init_simulate(PyObject *module)
{
}
