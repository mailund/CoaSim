/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

// must be included before anything else...
#ifndef PYTHON_H_INCLUDED
# include <Python.h>
# define PYTHON_H_INCLUDED
#endif

#ifndef PYTHON__ARG_HH_INCLUDED
# include "arg.hh"
#endif
#ifndef PYTHON__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif
#ifndef PYTHON__NODES_HH_INCLUDED
# include "nodes.hh"
#endif

#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif

struct NodeIteratorObject {
    PyObject_HEAD
    ARGObject *py_arg;

    typedef std::vector<core::Node*>::const_iterator itr_t;
    itr_t leaves_itr;
    itr_t leaves_end;
    itr_t inner_itr;
    itr_t inner_end;
};


static PyObject *
Node_iterator_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    NodeIteratorObject *self = (NodeIteratorObject *)type->tp_alloc(type, 0);
    self->py_arg = 0;
    return (PyObject*)self;
}

static int
Node_iterator_clear(NodeIteratorObject *self)
{
    Py_DECREF(self->py_arg);
    self->py_arg = 0;
    return 0;
}

static void
Node_iterator_dealloc(NodeIteratorObject *self)
{
    Node_iterator_clear(self);
    self->ob_type->tp_free((PyObject*)self);
}

static PyObject *
Node_iterator_iter(PyObject *self)
{
    Py_INCREF(self);
    return self;
}

static PyObject *
Node_iterator_iternext(PyObject *s)
{
    NodeIteratorObject *self = (NodeIteratorObject*)s;
    if (self->leaves_itr != self->leaves_end)
	return wrap_node(*(self->leaves_itr++), (PyObject*)self->py_arg);
    if (self->inner_itr != self->inner_end)
	return wrap_node(*(self->inner_itr++), (PyObject*)self->py_arg);
    return 0;
}


static PyTypeObject NodeIteratorType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.NodeIterator",	/*tp_name*/
    sizeof(NodeIteratorObject), /*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_iterator_dealloc,	/*tp_dealloc*/
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
    "Iterator over ARG nodes",	/* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    Node_iterator_iter,		// tp_iter
    Node_iterator_iternext,	// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    0,				// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Node_iterator_new,			// tp_new
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
sequences(ARGObject *self)
{
    const std::vector<core::Node*> &leaves = self->arg->leaves();
    std::vector<core::Node*>::const_iterator i;

    PyObject *sequences_list = PyList_New(leaves.size());
    for (i = leaves.begin(); i != leaves.end(); ++i)
	{
	    PyObject *sequence = PyList_New((*i)->no_states());
	    for (int j = 0; j < (*i)->no_states(); ++j)
		PyList_SetItem(sequence, j, PyInt_FromLong((*i)->state(j)));
	    PyList_SetItem(sequences_list, i - leaves.begin(), sequence);
	}

    return sequences_list;
}

static PyObject *
intervals(ARGObject *self)
{
    const std::vector<core::RetiredInterval> &rintervals
	= self->arg->retired_intervals();
    PyObject *interval_list = PyList_New(rintervals.size());
    std::vector<core::RetiredInterval>::const_iterator i;
    for (i = rintervals.begin(); i != rintervals.end(); ++i)
	{
	    PyObject *interval = wrap_interval(&(*i), self);
	    PyList_SetItem(interval_list, i - rintervals.begin(), interval);
	}
    return interval_list;
}

static PyObject *
nodes(ARGObject *self)
{
    NodeIteratorObject *node_itr =
	(NodeIteratorObject*)Node_iterator_new(&NodeIteratorType, 0, 0);
    if (!node_itr) return 0;

    Py_INCREF(self);
    node_itr->py_arg = self;

    const std::vector<core::Node*> &leaves = self->arg->leaves();
    const std::vector<core::Node*> &inner  = self->arg->inner_nodes();
    node_itr->leaves_itr = leaves.begin();
    node_itr->leaves_end = leaves.end();
    node_itr->inner_itr  = inner.begin();
    node_itr->inner_end  = inner.end();

    return (PyObject*)node_itr;
}


static PyGetSetDef ARG_getseters[] = {
    {"sequences", (getter)sequences, NULL,
     "Sequences found in the leaves of the ARG.",
     NULL /* no closure */
    },
    {"intervals", (getter)intervals, NULL,
     "Intervals sharing the same genealogy in the ARG.",
     NULL /* no closure */
    },
    {"nodes", (getter)nodes, NULL,
     "An iterator over the nodes in the ARG.",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyObject *
ARG_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    ARGObject *self = (ARGObject *)type->tp_alloc(type, 0);
    self->arg = 0;
    return (PyObject*)self;
}

static int
ARG_clear(ARGObject *self)
{
    delete self->arg;
    self->arg = 0;
    return 0;
}

static void
ARG_dealloc(ARGObject *self)
{
    ARG_clear(self);
    self->ob_type->tp_free((PyObject*)self);
}



static PyTypeObject ARGType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.ARG",		/*tp_name*/
    sizeof(ARGObject),		/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)ARG_dealloc,	/*tp_dealloc*/
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
    "Ancestral Recombination Graph (ARG)", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    ARG_getseters,		// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    ARG_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};




PyObject *wrap_arg(core::ARG *arg)
{
    ARGObject *py_arg = (ARGObject*)ARG_new(&ARGType, 0, 0);
    if (!py_arg) return 0;
    py_arg->arg = arg;
    return (PyObject*)py_arg;
}

void init_arg(PyObject *module)
{
    if (PyType_Ready(&ARGType) < 0) return;
    Py_INCREF(&ARGType);
    if (PyType_Ready(&NodeIteratorType) < 0) return;
    Py_INCREF(&NodeIteratorType);
}
