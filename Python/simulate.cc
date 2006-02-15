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
#ifndef CORE__BUILDER_EVENTS_HH_INCLUDED
# include <Core/builder_events.hh>
#endif
#ifndef CORE__SIMULATOR_HH_INCLUDED
# include <Core/simulator.hh>
#endif

static core::Event *
parse_event(PyObject *py_tuple)
{
    PyObject *handle = 0;
    static PyObject *bottleneck_handle = PyString_FromString("bottleneck");
    static PyObject *growth_handle     = PyString_FromString("growth");
    static PyObject *merge_handle      = PyString_FromString("merge");
    static PyObject *migration_handle  = PyString_FromString("migration");

    if (PyTuple_Size(py_tuple) < 1) goto error;
    handle = PyTuple_GetItem(py_tuple,0);
    if (!PyString_Check(handle)) goto error;

    if (_PyString_Eq(handle,bottleneck_handle) or _PyString_Eq(handle,growth_handle))
	{
	    if (PyTuple_Size(py_tuple) != 5) goto error;
	    PyObject *py_pop_number = PyTuple_GetItem(py_tuple,1);
	    PyObject *py_rel_size   = PyTuple_GetItem(py_tuple,2); // or beta
	    PyObject *py_start_time = PyTuple_GetItem(py_tuple,3);
	    PyObject *py_end_time   = PyTuple_GetItem(py_tuple,4);

	    if (!PyInt_Check(py_pop_number))   goto error;
	    if (!PyFloat_Check(py_rel_size))   goto error;
	    if (!PyFloat_Check(py_start_time)) goto error;
	    if (!PyFloat_Check(py_end_time))   goto error;

	    int pop_num = PyInt_AsLong(py_pop_number);
	    double rel_size = PyFloat_AsDouble(py_rel_size);
	    double start_time = PyFloat_AsDouble(py_start_time);
	    double end_time = PyFloat_AsDouble(py_end_time);

	    if (pop_num < 0)            goto error;
	    if (rel_size <= 0)          goto error;
	    if (start_time < 0)         goto error;
	    if (end_time <= start_time) goto error;

	    if (_PyString_Eq(handle,bottleneck_handle))
		return new core::BottleNeck(pop_num, rel_size, start_time, end_time);
	    else
		return new core::Growth(pop_num, rel_size, start_time, end_time);
	}
    else if (_PyString_Eq(handle,merge_handle))
	{
	    if (PyTuple_Size(py_tuple) != 3)   goto error;
	    PyObject *py_merge_time = PyTuple_GetItem(py_tuple,1);
	    PyObject *py_pop_list   = PyTuple_GetItem(py_tuple,2);
	    if (!PyFloat_Check(py_merge_time)) goto error;
	    if (!PyList_Check(py_pop_list))    goto error;

	    double merge_time = PyFloat_AS_DOUBLE(py_merge_time);
	    if (merge_time <= 0) goto error;
	    std::vector<int> pop_list;
	    for (int i = 0; i < PyList_Size(py_pop_list); ++i)
		{
		    PyObject *py_pop = PyList_GetItem(py_pop_list,i);
		    if (!PyInt_Check(py_pop)) goto error;
		    int pop = PyInt_AsLong(py_pop);
		    if (pop < 0) goto error;
		    pop_list.push_back(pop);
		}
	    return new core::PopulationMerge(pop_list.begin(), pop_list.end(),
					     merge_time);
	}
    else if (_PyString_Eq(handle,migration_handle))
	{
	    if (PyTuple_Size(py_tuple) != 5) goto error;
	    PyObject *py_src   = PyTuple_GetItem(py_tuple,1);
	    PyObject *py_dst   = PyTuple_GetItem(py_tuple,2);
	    PyObject *py_r     = PyTuple_GetItem(py_tuple,3);
	    PyObject *py_start = PyTuple_GetItem(py_tuple,4);
	    PyObject *py_end   = PyTuple_GetItem(py_tuple,5);
	    
	    if (!PyInt_Check(py_src))     goto error;
	    if (!PyInt_Check(py_dst))     goto error;
	    if (!PyFloat_Check(py_r))     goto error;
	    if (!PyFloat_Check(py_start)) goto error;
	    if (!PyFloat_Check(py_end))   goto error;

	    int src = PyInt_AsLong(py_src);
	    int dst = PyInt_AsLong(py_dst);
	    double r = PyFloat_AsDouble(py_r);
	    double start = PyFloat_AsDouble(py_start);
	    double end = PyFloat_AsDouble(py_end);

	    if (src < 0 or dst < 0) goto error;
	    if (r < 0)              goto error;
	    if (start < 0)          goto error;
	    if (end <= start)       goto error;

	    return new core::Migration(src,dst,r,start,end);
	}
    else
	; // fall through to error!

 error:
    PyErr_SetString(PyExc_ValueError, "arg #3 contains a malformed event.");
    return 0;
}

namespace {
    struct DeletingEventVector : public std::vector<core::Event*> {
	~DeletingEventVector();
    };
    DeletingEventVector::~DeletingEventVector()
    {
	for (const_iterator i = begin(); i != end(); ++i)
	    delete *i;
    }
}

PyObject *
simulate(PyObject *self, PyObject *args, PyObject *kwds)
{
    // FIXME: handle callbacks

    PyObject *py_markers;
    PyObject *py_sample_sizes;
    PyObject *py_events;

    std::vector<int> sample_sizes;
    std::vector<core::Marker*> markers;
    DeletingEventVector events;

    double rho = 0;
    double Q = 0;
    double gamma = 0;
    double beta = 0;

    PyObject *py_keep_empty = Py_False;
    long seed = 0;

    char *kwd_list[] = {
	"markers", "sampleSizes", "events",
	"rho", "Q", "gamma", "beta", 
	"keepEmptyIntervals",
	"seed",
	NULL
    };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, 
				      "O!O!O!|ddddO!l", kwd_list,
				      &PyList_Type, &py_markers,
				      &PyList_Type, &py_sample_sizes,
				      &PyList_Type, &py_events,
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

    for (int i = 0; i < PyList_Size(py_sample_sizes); ++i)
	{
	    PyObject *py_ss = PyList_GetItem(py_sample_sizes, i);
	    if (!PyInt_Check(py_ss))
		{
		    PyErr_SetString(PyExc_TypeError, "arg #2 contains a non-integer");
		    return 0;
		}
	    sample_sizes.push_back(PyInt_AsLong(py_ss));
	}

    for (int i = 0; i < PyList_Size(py_events); ++i)
	{
	    PyObject *py_event = PyList_GetItem(py_events, i);
	    if (!PyTuple_Check(py_event))
		{
		    PyErr_SetString(PyExc_TypeError, 
				    "arg #3 contains an unexpected event type");
		    return 0;
		}
	    core::Event *core_event = parse_event(py_event);
	    if (!core_event) return 0;
	    events.push_back(core_event);
	}


    core::ARG *arg = 0;
    try {
	core::Configuration conf(sample_sizes.begin(), sample_sizes.end(),
				 markers.begin(), markers.end(),
				 events.begin(),  events.end(),
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
