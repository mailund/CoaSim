/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__MARKERS_HH_INCLUDED
# include "markers.hh"
#endif

#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
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
    PyObject *string = NULL;
    PyObject *terminal = NULL;

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

PyTypeObject SNPMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.SNPMarker",		/*tp_name*/
    sizeof(SNPMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)SNPMarker_dealloc, /*tp_dealloc*/
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
    PyObject *string = NULL;
    PyObject *terminal = NULL;

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

PyTypeObject TraitMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.TraitMarker",		/*tp_name*/
    sizeof(TraitMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)TraitMarker_dealloc, /*tp_dealloc*/
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
    PyObject *string = NULL;
    PyObject *terminal = NULL;

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

PyTypeObject MicroSatelliteMarkerType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.MicroSatelliteMarker", /*tp_name*/
    sizeof(MicroSatelliteMarkerObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)MicroSatelliteMarker_dealloc, /*tp_dealloc*/
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

// FIXME: custom marker




void init_markers(PyObject *module)
{
    if (PyType_Ready(&MarkerType) < 0) return;
    Py_INCREF(&MarkerType);

    SNPMarkerType.tp_base = &MarkerType;
    TraitMarkerType.tp_base = &MarkerType;
    MicroSatelliteMarkerType.tp_base = &MarkerType;

    if (PyType_Ready(&SNPMarkerType) < 0) return;
    Py_INCREF(&SNPMarkerType);
    if (PyType_Ready(&TraitMarkerType) < 0) return;
    Py_INCREF(&TraitMarkerType);
    if (PyType_Ready(&MicroSatelliteMarkerType) < 0) return;
    Py_INCREF(&MicroSatelliteMarkerType);


    PyModule_AddObject(module, "SNPMarker", (PyObject *)&SNPMarkerType);
    PyModule_AddObject(module, "TraitMarker", (PyObject *)&TraitMarkerType);
    PyModule_AddObject(module, "MicroSatelliteMarker", 
		       (PyObject *)&MicroSatelliteMarkerType);
}
