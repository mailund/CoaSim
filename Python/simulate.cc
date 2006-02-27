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
#ifndef PYTHON__EXCEPTIONS_HH_INCLUDED
# include "exceptions.hh"
#endif
#ifndef PYTHON__NODES_HH_INCLUDED
# include "nodes.hh"
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
#ifndef CORE__BUILDER_HH_INCLUDED
# include <Core/builder.hh>
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
	    if (PyTuple_Size(py_tuple) != 6) goto error;
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

    class Callbacks : public core::BuilderMonitor {
	PyObject *i_py_callback;

	bool i_has_coa_cb;
	bool i_has_rc_cb;
	bool i_has_gc_cb;

	bool i_has_bottleneck_cb;
	bool i_has_growth_cb;

	bool i_has_migration_cb;
	bool i_has_pop_merge_cb;

    public:
	Callbacks(PyObject *py_callback);
	virtual ~Callbacks();
	
	virtual void coalescence_callback(core::CoalescentNode *n, int k);
	virtual void recombination_callback(core::RecombinationNode *n1,
					    core::RecombinationNode *n2,
					    int k);
	virtual void gene_conversion_callback(core::GeneConversionNode *n1,
					      core::GeneConversionNode *n2,
					      int k);
	
	virtual void bottleneck_callback(int pop, bool entering,
					 double time, int k);
	virtual void growth_callback(int pop, bool entering,
				     double time, int k);
	
	virtual void migration_callback(int pop1, int pop2,
					double time, int k);
	virtual void population_merge_callback(const std::vector<int> &pops,
					       double time, int k);
    };

    Callbacks::Callbacks(PyObject *py_callback) 
	: i_py_callback(py_callback),
	  i_has_coa_cb(PyObject_HasAttrString(py_callback,"coalescentEvent")),
	  i_has_rc_cb(PyObject_HasAttrString(py_callback,"recombinationEvent")), 
	  i_has_gc_cb(PyObject_HasAttrString(py_callback,"geneConversionEvent")),
	  i_has_bottleneck_cb(PyObject_HasAttrString(py_callback,"bottleneckEvent")),
	  i_has_growth_cb(PyObject_HasAttrString(py_callback,"growthEvent")),
	  i_has_migration_cb(PyObject_HasAttrString(py_callback,"migrationEvent")),
	  i_has_pop_merge_cb(PyObject_HasAttrString(py_callback,"mergeEvent"))
    {
	Py_INCREF(i_py_callback);
	// FIXME: set bool flags
    }

    Callbacks::~Callbacks()
    {
	Py_DECREF(i_py_callback);
    }

    void Callbacks::coalescence_callback(core::CoalescentNode *n, int k)
    {
	if (!i_has_coa_cb) return;
	// fake ARG (which doesn't exist yet) with None
	PyObject *py_node = wrap_node(n, Py_None);
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "coalescentEvent", "Oi", py_node, k); 
	Py_DECREF(py_node);
	if (!py_result) throw PyException();
    }

    void Callbacks::recombination_callback(core::RecombinationNode *n1,
					   core::RecombinationNode *n2,
					   int k)
    {
	if (!i_has_rc_cb) return;
	// fake ARG (which doesn't exist yet) with None
	PyObject *py_n1 = wrap_node(n1, Py_None);
	PyObject *py_n2 = wrap_node(n2, Py_None);
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "recombinationEvent", 
				"OOi", py_n1, py_n2, k); 
	Py_DECREF(py_n1); Py_DECREF(py_n2);
	if (!py_result) throw PyException();
    }

    void Callbacks::gene_conversion_callback(core::GeneConversionNode *n1,
					     core::GeneConversionNode *n2,
					     int k)
    {
	if (!i_has_gc_cb) return;
	// fake ARG (which doesn't exist yet) with None
	PyObject *py_n1 = wrap_node(n1, Py_None);
	PyObject *py_n2 = wrap_node(n2, Py_None);
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "geneConversionEvent", 
				"OOi", py_n1, py_n2, k); 
	Py_DECREF(py_n1); Py_DECREF(py_n2);
	if (!py_result) throw PyException();
    }


    void Callbacks::bottleneck_callback(int pop, bool entering, double time, int k)
    {
	if (!i_has_bottleneck_cb) return;
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "bottleneckEvent", 
				"iOdi", pop, (entering ? Py_True : Py_False),
				time, k);
	if (!py_result) throw PyException();
    }
    
    void Callbacks::growth_callback(int pop, bool entering, double time, int k)
    {
	if (!i_has_growth_cb) return;
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "growthEvent", 
				"iOdi", pop, (entering ? Py_True : Py_False),
				time, k);
	if (!py_result) throw PyException();
    }
    
    
    void Callbacks::migration_callback(int pop1, int pop2, double time, int k)
    {
	if (!i_has_migration_cb) return;
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "migrationEvent", 
				"iidi", pop1, pop2, time, k);
	if (!py_result) throw PyException();
    }
    
    void Callbacks::population_merge_callback(const std::vector<int> &pops,
					      double time, int k)
    {
	if (!i_has_pop_merge_cb) return;
	PyObject *py_list = PyList_New(pops.size());
	if (!py_list) throw PyException();
	std::vector<int>::const_iterator itr; int i;
	for (i = 0, itr = pops.begin(); itr != pops.end(); ++itr, ++i)
	    PyList_SetItem(py_list, i, Py_BuildValue("i", *itr));
	PyObject *py_result = 
	    PyObject_CallMethod(i_py_callback, "mergeEvent", 
				"Odi", py_list, time, k);
	Py_DECREF(py_list);
	if (!py_result) throw PyException();
    }

}

PyObject *
simulate(PyObject *self, PyObject *args, PyObject *kwds)
{
    // FIXME: handle callbacks

    PyObject *py_markers;
    PyObject *py_sample_sizes;
    PyObject *py_events;
    PyObject *py_callbacks;

    std::vector<int> sample_sizes;
    std::vector<core::Marker*> markers;
    DeletingEventVector events;

    double rho = 0;
    double Q = 0;
    double gamma = 0;
    double beta = 0;

    PyObject *py_keep_empty = Py_False;
    PyObject *py_keep_migration_events = Py_False;
    long seed = 0;

    char *kwd_list[] = {
	"markers", "sampleSizes", "events",
	"rho", "Q", "gamma", "beta", 
	"callbacks",
	"keepEmptyIntervals",
	"keepMigrationEvents",
	"seed",
	NULL
    };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, 
				      "O!O!O!|ddddOO!O!l", kwd_list,
				      &PyList_Type, &py_markers,
				      &PyList_Type, &py_sample_sizes,
				      &PyList_Type, &py_events,
				      &rho, &Q, &gamma, &beta,
				      &py_callbacks,
				      &PyBool_Type, &py_keep_empty,
				      &PyBool_Type, &py_keep_migration_events,
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

    std::auto_ptr<Callbacks> callbacks(py_callbacks==Py_None ? 0 :
				       new Callbacks(py_callbacks));

    core::ARG *arg = 0;
    try {
	core::Configuration conf(sample_sizes.begin(), sample_sizes.end(),
				 markers.begin(), markers.end(),
				 events.begin(),  events.end(),
				 rho, Q, gamma, beta);
	arg = core::Simulator::simulate(conf, callbacks.get(),
					py_keep_empty==Py_True,
					py_keep_migration_events==Py_True,
					seed);

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
