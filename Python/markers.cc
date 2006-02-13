/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__MARKERS_HH_INCLUDED
# include "markers.hh"
#endif
#ifndef PYTHON__EXCEPTIONS_HH_INCLUDED
# include "exceptions.hh"
#endif

#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif

#ifndef SSTREAM_INCLUDED
# include <sstream>
# define SSTREAM_INCLUDED
#endif



static PyObject *
Marker_position_get(MarkerObject *self)
{
    return Py_BuildValue("d", self->core_marker->position());
}

static int
Marker_position_set(MarkerObject *self, PyObject *arg)
{
    double pos;
    
    if (!PyFloat_Check(arg))
	{
	    PyErr_SetString(PyExc_TypeError, 
			    "Position must be a float.");
	    return -1;
	}

    self->core_marker->position(PyFloat_AsDouble(arg));
    return 0;
}

static PyGetSetDef Marker_getseters[] = {
    {"position", (getter)Marker_position_get, (setter)Marker_position_set,
     "Position of the marker on the genomic region.",
     NULL /* no closure */
    },

    {0}				// sentinel
};


static PyObject *
Marker_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    MarkerObject *self = (MarkerObject *)type->tp_alloc(type, 0);
    self->core_marker = 0;
    return (PyObject*)self;
}

static int
Marker_clear(MarkerObject *self)
{
    delete self->core_marker;
    self->core_marker = 0;
    return 0;
}

static void
Marker_dealloc(MarkerObject *self)
{
    Marker_clear(self);
    self->ob_type->tp_free((PyObject*)self);
}

PyTypeObject MarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.Marker",		/*tp_name*/
    sizeof(MarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Marker_dealloc, /*tp_dealloc*/
    0,				/*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    0,				/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    0,				/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "Abstract base class for markers.",	/* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    Marker_getseters,		// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Marker_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};





static PyObject *
SNPMarker_lowFreq(SNPMarkerObject *self)
{
    core::SNPMarker *core_marker 
	= dynamic_cast<core::SNPMarker*>(self->base.core_marker);
    return Py_BuildValue("d", core_marker->low_freq());
}

static PyObject *
SNPMarker_highFreq(SNPMarkerObject *self)
{
    core::SNPMarker *core_marker 
	= dynamic_cast<core::SNPMarker*>(self->base.core_marker);
    return Py_BuildValue("d", core_marker->high_freq());
}

static PyGetSetDef SNPMarker_getseters[] = {
    {"lowFreq", (getter)SNPMarker_lowFreq, NULL,
     "Lowest acceptable mutant allele frequency.",
     NULL /* no closure */
    },
    {"highFreq", (getter)SNPMarker_highFreq, NULL,
     "Highest acceptable mutant allele frequency.",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyObject *
SNPMarker_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    SNPMarkerObject *self = (SNPMarkerObject *)type->tp_alloc(type, 0);
    return (PyObject*)self;
}

static int
SNPMarker_init(SNPMarkerObject *self, PyObject *args)
{
    double position;
    double low_freq;
    double high_freq;

    if (! PyArg_ParseTuple(args, "ddd", &position, &low_freq, &high_freq))
        return -1; 		/* rethrow exception */

    core::SNPMarker *core_marker = 0;
    try {
	core_marker = new core::SNPMarker(position, low_freq, high_freq);
    } catch (core::Marker::illegal_value &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (core::Marker::illegal_position &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (std::exception &ex) {
	PyErr_SetString(PyExc_RuntimeError, ex.what());
	return -1;
    }

    self->base.core_marker = core_marker;
    return 0;
}

static int
SNPMarker_clear(SNPMarkerObject *self)
{
    return 0;
}

static void
SNPMarker_dealloc(SNPMarkerObject *self)
{
    SNPMarker_clear(self);
    self->base.ob_type->tp_free((PyObject*)self);
}

static PyObject *
SNPMarker_str(SNPMarkerObject *self)
{
    core::SNPMarker *core_marker 
	= dynamic_cast<core::SNPMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "SNPMarker(" << core_marker->position()
       << ',' << core_marker->low_freq()
       << ',' << core_marker->high_freq()
       << ')';
    return PyString_FromString(os.str().c_str());
}

static int
SNPMarker_print(SNPMarkerObject *self, FILE *fp, int flags)
{
    core::SNPMarker *core_marker 
	= dynamic_cast<core::SNPMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "SNPMarker(" << core_marker->position()
       << ',' << core_marker->low_freq()
       << ',' << core_marker->high_freq()
       << ')';
    fprintf(fp, os.str().c_str());
    return 0;
}


PyTypeObject SNPMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.SNPMarker",		/*tp_name*/
    sizeof(SNPMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)SNPMarker_dealloc, /*tp_dealloc*/
    (printfunc)SNPMarker_print,	/*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    0,				/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    (reprfunc)SNPMarker_str,	/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT,		/*tp_flags*/
    "Single Nucleotide Polymorphism (SNP) marker.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    SNPMarker_getseters,	// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    (initproc)SNPMarker_init,	// tp_init
    0,				// tp_alloc
    SNPMarker_new,		// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};







static PyObject *
TraitMarker_lowFreq(TraitMarkerObject *self)
{
    core::TraitMarker *core_marker 
	= dynamic_cast<core::TraitMarker*>(self->base.core_marker);
    return Py_BuildValue("d", core_marker->low_freq());
}

static PyObject *
TraitMarker_highFreq(TraitMarkerObject *self)
{
    core::TraitMarker *core_marker 
	= dynamic_cast<core::TraitMarker*>(self->base.core_marker);
    return Py_BuildValue("d", core_marker->high_freq());
}

static PyGetSetDef TraitMarker_getseters[] = {
    {"lowFreq", (getter)TraitMarker_lowFreq, NULL,
     "Lowest acceptable mutant allele frequency.",
     NULL /* no closure */
    },
    {"highFreq", (getter)TraitMarker_highFreq, NULL,
     "Highest acceptable mutant allele frequency.",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyObject *
TraitMarker_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    TraitMarkerObject *self = (TraitMarkerObject *)type->tp_alloc(type, 0);
    return (PyObject*)self;
}

static int
TraitMarker_init(TraitMarkerObject *self, PyObject *args)
{
    double position;
    double low_freq;
    double high_freq;

    if (! PyArg_ParseTuple(args, "ddd", &position, &low_freq, &high_freq))
        return -1; 		/* rethrow exception */

    core::TraitMarker *core_marker = 0;
    try {
	core_marker = new core::TraitMarker(position, low_freq, high_freq);
    } catch (core::Marker::illegal_value &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (core::Marker::illegal_position &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (std::exception &ex) {
	PyErr_SetString(PyExc_RuntimeError, ex.what());
	return -1;
    }

    self->base.core_marker = core_marker;
    return 0;
}

static int
TraitMarker_clear(TraitMarkerObject *self)
{
    return 0;
}

static void
TraitMarker_dealloc(TraitMarkerObject *self)
{
    TraitMarker_clear(self);
    self->base.ob_type->tp_free((PyObject*)self);
}

static PyObject *
TraitMarker_str(TraitMarkerObject *self)
{
    core::TraitMarker *core_marker 
	= dynamic_cast<core::TraitMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "TraitMarker(" << core_marker->position()
       << ',' << core_marker->low_freq()
       << ',' << core_marker->high_freq()
       << ')';
    return PyString_FromString(os.str().c_str());
}

static int
TraitMarker_print(TraitMarkerObject *self, FILE *fp, int flags)
{
    core::TraitMarker *core_marker 
	= dynamic_cast<core::TraitMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "TraitMarker(" << core_marker->position()
       << ',' << core_marker->low_freq()
       << ',' << core_marker->high_freq()
       << ')';
    fprintf(fp, os.str().c_str());
    return 0;
}

PyTypeObject TraitMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.TraitMarker",		/*tp_name*/
    sizeof(TraitMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)TraitMarker_dealloc, /*tp_dealloc*/
    (printfunc)TraitMarker_print, /*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    0,				/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    (reprfunc)TraitMarker_str,	/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT,		/*tp_flags*/
    "Trait marker.",		/* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    TraitMarker_getseters,	// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    (initproc)TraitMarker_init,	// tp_init
    0,				// tp_alloc
    TraitMarker_new,		// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};








static PyObject *
MicroSatelliteMarker_theta(MicroSatelliteMarkerObject *self)
{
    core::MicroSatelliteMarker *core_marker 
	= dynamic_cast<core::MicroSatelliteMarker*>(self->base.core_marker);
    return Py_BuildValue("d", core_marker->theta());
}

static PyObject *
MicroSatelliteMarker_K(MicroSatelliteMarkerObject *self)
{
    core::MicroSatelliteMarker *core_marker 
	= dynamic_cast<core::MicroSatelliteMarker*>(self->base.core_marker);
    return Py_BuildValue("i", core_marker->K());
}

static PyGetSetDef MicroSatelliteMarker_getseters[] = {
    {"theta", (getter)MicroSatelliteMarker_theta, NULL,
     "Mutation rate.",
     NULL /* no closure */
    },
    {"K", (getter)MicroSatelliteMarker_K, NULL,
     "Number of alleles.",
     NULL /* no closure */
    },
    {0}				// sentinel
};

static PyObject *
MicroSatelliteMarker_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    MicroSatelliteMarkerObject *self 
	= (MicroSatelliteMarkerObject *)type->tp_alloc(type, 0);
    return (PyObject*)self;
}

static int
MicroSatelliteMarker_init(MicroSatelliteMarkerObject *self, PyObject *args)
{
    double position;
    double mu;
    long k;

    if (! PyArg_ParseTuple(args, "ddl", &position, &mu, &k))
        return -1; 		/* rethrow exception */

    core::MicroSatelliteMarker *core_marker = 0;
    try {
	core_marker = new core::MicroSatelliteMarker(position, mu, k);
    } catch (core::Marker::illegal_value &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (core::Marker::illegal_position &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (std::exception &ex) {
	PyErr_SetString(PyExc_RuntimeError, ex.what());
	return -1;
    }

    self->base.core_marker = core_marker;
    return 0;
}

static int
MicroSatelliteMarker_clear(MicroSatelliteMarkerObject *self)
{
    return 0;
}

static void
MicroSatelliteMarker_dealloc(MicroSatelliteMarkerObject *self)
{
    MicroSatelliteMarker_clear(self);
    self->base.ob_type->tp_free((PyObject*)self);
}

static PyObject *
MicroSatelliteMarker_str(MicroSatelliteMarkerObject *self)
{
    core::MicroSatelliteMarker *core_marker 
	= dynamic_cast<core::MicroSatelliteMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "MicroSatelliteMarker(" << core_marker->position()
       << ',' << core_marker->theta()
       << ',' << core_marker->K()
       << ')';
    return PyString_FromString(os.str().c_str());
}

static int
MicroSatelliteMarker_print(TraitMarkerObject *self, FILE *fp, int flags)
{
    core::MicroSatelliteMarker *core_marker 
	= dynamic_cast<core::MicroSatelliteMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "MicroSatelliteMarker(" << core_marker->position()
       << ',' << core_marker->theta()
       << ',' << core_marker->K()
       << ')';
    fprintf(fp, os.str().c_str());
    return 0;
}


PyTypeObject MicroSatelliteMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.MicroSatelliteMarker", /*tp_name*/
    sizeof(MicroSatelliteMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)MicroSatelliteMarker_dealloc, /*tp_dealloc*/
    (printfunc)MicroSatelliteMarker_print, /*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    0,				/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    (reprfunc)MicroSatelliteMarker_str,	/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT,		/*tp_flags*/
    "MicroSatellite marker.",	/* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    MicroSatelliteMarker_getseters,	// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    (initproc)MicroSatelliteMarker_init, // tp_init
    0,				// tp_alloc
    MicroSatelliteMarker_new,	// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};

namespace {
    using namespace core;
    struct PythonMarker : public core::Marker {
	PyObject *py_marker;
	// in simulations we copy markers into the simulator, and in
	// those cases we cannot let the python object be deallocated
	// before the simulation is done, so we keep a reference count
	// to it -- on the other hand, when we creat the object from
	// python, we *don't* want a reference, 'cause then it would
	// never be deallocated again, thus we keep track of if it is
	// a copy (that should be decref'ed) or not.
	bool py_marker_copied;

	// polymorphic copying
	virtual Marker *copy() const;

	virtual bool run_first() const;
	virtual int default_value() const;

	// creates a new mutator -- the mutator must be deleted after use.
	virtual Mutator *create_mutator(const Configuration &conf,
					const RetiredInterval &ri) const;

	virtual const char * type() const;

    public:

	PythonMarker(double position, PyObject *py_marker) 
	    : core::Marker(position), py_marker(py_marker),
	      py_marker_copied(false)
	{
	}
	PythonMarker(const PythonMarker &pm)
	    : core::Marker(pm.position()), py_marker(pm.py_marker),
	      py_marker_copied(true)
	{
	    Py_INCREF(py_marker);
	}
	~PythonMarker();
    };

    PythonMarker::~PythonMarker()
    {
	if (py_marker_copied)
	    {
		Py_DECREF(py_marker);
	    }
    }

    class PythonMutator : public core::Mutator {
	const PythonMarker &i_marker;
	int mutate(const Node &parent, const Node &child, int parent_allele);

    public:
	PythonMutator(const PythonMarker &marker) : i_marker(marker) {}
    };

    Marker *PythonMarker::copy() const
    {
	return new PythonMarker(*this);
    }

    bool PythonMarker::run_first() const
    {
	return false;  // FIXME
    }

    
    const char *PythonMarker::type() const 
    {
	return "PythonMarker"; 
    }

    int PythonMarker::default_value() const
    {
	PyObject *py_value = PyObject_CallMethod(py_marker, "defaultValue", 0);
	if (!py_value) throw PyException();
	if (!PyInt_Check(py_value)) 
	    {
		PyErr_SetString(PyExc_TypeError,
				"defaultValue() must return an integer.");
		throw PyException();
	    }
	return PyInt_AS_LONG(py_value);
    }

    Mutator *PythonMarker::create_mutator(const Configuration &conf,
					  const RetiredInterval &ri) const
    {
	return new PythonMutator(*this);
    }

    int PythonMutator::mutate(const Node &parent, const Node &child, 
			      int parent_allele)
    {
	double edge_length = parent.time() - child.time();
	PyObject *py_value = PyObject_CallMethod(i_marker.py_marker, "mutate",
						 "id", parent_allele, edge_length);
	if (!py_value) throw PyException();
	if (!PyInt_Check(py_value)) 
	    {
		PyErr_SetString(PyExc_TypeError,
				"mutate() must return an integer.");
		throw PyException();
	    }
	return PyInt_AS_LONG(py_value);
    }
}



static PyGetSetDef PythonMarker_getseters[] = {
    {0}				// sentinel
};

static PyObject *
PythonMarker_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    PythonMarkerObject *self = (PythonMarkerObject *)type->tp_alloc(type, 0);
    return (PyObject*)self;
}

static int
PythonMarker_init(PythonMarkerObject *self, PyObject *args)
{
    double position = 0;

    if (! PyArg_ParseTuple(args, "d", &position))
        return -1; 		/* rethrow exception */

    PythonMarker *core_marker = 0;
    try {
	core_marker = new PythonMarker(position, (PyObject*)self);
    } catch (core::Marker::illegal_position &ex) {
	PyErr_SetString(PyExc_ValueError, ex.what());
	return -1;
    } catch (std::exception &ex) {
	PyErr_SetString(PyExc_RuntimeError, ex.what());
	return -1;
    }

    self->base.core_marker = core_marker;
    return 0;
}

static int
PythonMarker_clear(PythonMarkerObject *self)
{
    return 0;
}

static void
PythonMarker_dealloc(PythonMarkerObject *self)
{
    PythonMarker_clear(self);
    self->base.ob_type->tp_free((PyObject*)self);
}

static PyObject *
PythonMarker_str(PythonMarkerObject *self)
{
    PythonMarker *core_marker = dynamic_cast<PythonMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "PythonMarker(" << core_marker->position() << ')';
    return PyString_FromString(os.str().c_str());
}

static int
PythonMarker_print(PythonMarkerObject *self, FILE *fp, int flags)
{
    PythonMarker *core_marker = dynamic_cast<PythonMarker*>(self->base.core_marker);
    std::ostringstream os; 
    os << "PythonMarker(" << core_marker->position() << ')';
    fprintf(fp, os.str().c_str());
    return 0;
}


PyTypeObject PythonMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.PythonMarker", /*tp_name*/
    sizeof(PythonMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)PythonMarker_dealloc, /*tp_dealloc*/
    (printfunc)PythonMarker_print, /*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    0,				/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    (reprfunc)PythonMarker_str,	/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "Custom marker.",		/* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    PythonMarker_getseters,	// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    (initproc)PythonMarker_init, // tp_init
    0,				// tp_alloc
    PythonMarker_new,		// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};




void init_markers(PyObject *module)
{
    if (PyType_Ready(&MarkerType) < 0) return;
    Py_INCREF(&MarkerType);

    SNPMarkerType.tp_base = &MarkerType;
    TraitMarkerType.tp_base = &MarkerType;
    MicroSatelliteMarkerType.tp_base = &MarkerType;
    PythonMarkerType.tp_base = &MarkerType;

    if (PyType_Ready(&SNPMarkerType) < 0) return;
    Py_INCREF(&SNPMarkerType);
    if (PyType_Ready(&TraitMarkerType) < 0) return;
    Py_INCREF(&TraitMarkerType);
    if (PyType_Ready(&MicroSatelliteMarkerType) < 0) return;
    Py_INCREF(&MicroSatelliteMarkerType);
    if (PyType_Ready(&PythonMarkerType) < 0) return;
    Py_INCREF(&PythonMarkerType);


    PyModule_AddObject(module, "Marker", (PyObject *)&MarkerType);
    PyModule_AddObject(module, "SNPMarker", (PyObject *)&SNPMarkerType);
    PyModule_AddObject(module, "TraitMarker", (PyObject *)&TraitMarkerType);
    PyModule_AddObject(module, "MicroSatelliteMarker", 
		       (PyObject *)&MicroSatelliteMarkerType);
    PyModule_AddObject(module, "CustomMarker", (PyObject *)&PythonMarkerType);
}
