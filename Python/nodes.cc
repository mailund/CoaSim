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

#ifndef PYTHON__NODES_HH_INCLUDED
# include "nodes.hh"
#endif
#ifndef PYTHON__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif

#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif

static PyObject *
event_time(NodeObject *self)
{
    return Py_BuildValue("d", self->core_node->time());
}

static PyObject *
Node_children(NodeObject *self)
{
    PyErr_SetString(PyExc_NotImplementedError,
		    "children is not implemented in the abstract node class.");
    return 0;
}


static PyGetSetDef Node_getseters[] = {
    {"eventTime", (getter)event_time, NULL,
     "The time of the event represented by the node (measured in units of "
     "2Ne).",
     NULL /* no closure */
    },
    {"children", (getter)Node_children, NULL,
     "The children of this node.",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyObject *
is_ancestral(NodeObject *self, PyObject *args)
{
    double point;
    if (! PyArg_ParseTuple(args, "d:isAncestral", &point)) return 0;
    return PyBool_FromLong(self->core_node->contains_point(point));
}

static PyObject *
is_trapped(NodeObject *self, PyObject *args)
{
    double point;
    if (! PyArg_ParseTuple(args, "d:isTrapped", &point)) return 0;
    bool trapped =  
	!(self->core_node->contains_point(point)) and
	(self->core_node->intervals().first_point() <= point) and
	(point < self->core_node->intervals().last_point());
    return PyBool_FromLong(trapped);
}

static PyMethodDef Node_methods[] = {
    {"isAncestral", (PyCFunction)is_ancestral, METH_VARARGS,
     "isAncetral(p)\n\n"
     "A predicate that tests if point p is ancestral in this node."
    },
    {"isTrapped", (PyCFunction)is_ancestral, METH_VARARGS,
     "isTralled(p)\n\n"
     "A predicate that tests if point p is in trapped material in this node."
    },
    {0}  /* Sentinel */
};

static PyObject *
Node_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    NodeObject *self = (NodeObject *)type->tp_alloc(type, 0);
    self->py_arg = 0;
    self->core_node = 0;
    return (PyObject*)self;
}

static int
Node_init(PyObject *self, PyObject *args, PyObject *kwargs)
{
    PyErr_SetString(PyExc_RuntimeError, 
		    "Nodes should not be explicitly initialized!");
    return -1;
}


static int
Node_clear(NodeObject *self)
{
    Py_XDECREF(self->py_arg);
    self->py_arg = 0;
    self->core_node = 0;
    return 0;
}

static void
Node_dealloc(NodeObject *self)
{
    Node_clear(self);
    self->ob_type->tp_free((PyObject*)self);
}

static int
Node_compare(NodeObject *n1, NodeObject *n2)
{
    if (n1->core_node == n2->core_node) return 0;
    return -1;
}



static PyTypeObject NodeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.Node",		/*tp_name*/
    sizeof(NodeObject),		/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_dealloc,	/*tp_dealloc*/
    0,				/*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    (cmpfunc)Node_compare,	/*tp_compare*/
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
    "An (abstract) node in the ARG.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    Node_methods,		// tp_methods
    0,				// tp_members
    Node_getseters,		// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    Node_init,			// tp_init
    0,				// tp_alloc
    Node_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};





// Leaf nodes --------------------------------------------
static PyObject *
LeafNode_children(NodeObject *self)
{
    return PyList_New(0);
}

static PyGetSetDef LeafNode_getseters[] = {
    {"children", (getter)LeafNode_children, NULL,
     "The children of this node.",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyTypeObject LeafNodeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.LeafNode",		/*tp_name*/
    sizeof(LeafNodeObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_dealloc,	/*tp_dealloc*/
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
    "A leaf node in the ARG.",	/* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    LeafNode_getseters,		// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Node_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};


// Coalescent nodes --------------------------------------------
static PyObject *
CoalescentNode_children(NodeObject *self)
{
    const core::CoalescentNode *n
	= dynamic_cast<const core::CoalescentNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("(OO)", 
			 wrap_node(n->left_child(), self->py_arg),
			 wrap_node(n->right_child(), self->py_arg));
}

static PyGetSetDef CoalescentNode_getseters[] = {
    {"children", (getter)CoalescentNode_children, NULL,
     "The children of this node.",
     NULL /* no closure */
    },
    {0}				// sentinel
};

static PyTypeObject CoalescentNodeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.CoalescentNode",	/*tp_name*/
    sizeof(CoalescentNodeObject), /*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_dealloc,	/*tp_dealloc*/
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
    "A coalescent node in the ARG.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    CoalescentNode_getseters,	// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Node_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};


// Recombination nodes --------------------------------------------
static PyObject *
RecombinationNode_children(NodeObject *self)
{
    const core::RecombinationNode *n
	= dynamic_cast<const core::RecombinationNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("(O)", wrap_node(n->child(),self->py_arg));
}

static PyObject *
recombination_point(NodeObject *self)
{
    const core::RecombinationNode *n
	= dynamic_cast<const core::RecombinationNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("d", n->cross_over_point());
}


static PyGetSetDef RecombinationNode_getseters[] = {
    {"children", (getter)RecombinationNode_children, NULL,
     "The children of this node.",
     NULL /* no closure */
    },
    {"recombinationPoint", (getter)recombination_point, NULL,
     "The cross-over point for the recombination.",
     NULL /* no closure */
    },
    {0}				// sentinel
};

static PyTypeObject RecombinationNodeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.RecombinationNode",	/*tp_name*/
    sizeof(RecombinationNodeObject), /*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_dealloc,	/*tp_dealloc*/
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
    "A recombination node in the ARG.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    RecombinationNode_getseters, // tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Node_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};


// GeneConversion nodes --------------------------------------------
static PyObject *
GeneConversionNode_children(NodeObject *self)
{
    const core::GeneConversionNode *n
	= dynamic_cast<const core::GeneConversionNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("(O)", wrap_node(n->child(),self->py_arg));
}

static PyObject *
gene_conversion_interval(NodeObject *self)
{
    const core::GeneConversionNode *n
	= dynamic_cast<const core::GeneConversionNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("(dd)", n->conversion_start(), n->conversion_end());
}

static PyGetSetDef GeneConversionNode_getseters[] = {
    {"children", (getter)GeneConversionNode_children, NULL,
     "The children of this node.",
     NULL /* no closure */
    },
    {"conversionInterval", (getter)gene_conversion_interval, NULL,
     "The interval where the gene-conversion occured.",
     NULL /* no closure */
    },
    {0}				// sentinel
};

static PyTypeObject GeneConversionNodeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.GeneConversionNode",	/*tp_name*/
    sizeof(GeneConversionNodeObject), /*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_dealloc,	/*tp_dealloc*/
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
    "A gene-conversion node in the ARG.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    GeneConversionNode_getseters, // tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Node_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};



// Migration nodes --------------------------------------------
static PyObject *
MigrationNode_children(NodeObject *self)
{
    const core::MigrationNode *n
	= dynamic_cast<const core::MigrationNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("(O)", wrap_node(n->child(),self->py_arg));
}

static PyObject *
MigrationNode_source_population(NodeObject *self)
{
    const core::MigrationNode *n
	= dynamic_cast<const core::MigrationNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("i", n->source_population());
}

static PyObject *
MigrationNode_destination_population(NodeObject *self)
{
    const core::MigrationNode *n
	= dynamic_cast<const core::MigrationNode*>(self->core_node);
    assert(n);
    return Py_BuildValue("i", n->destination_population());
}


static PyGetSetDef MigrationNode_getseters[] = {
    {"children", (getter)MigrationNode_children, NULL,
     "The children of this node.",
     NULL /* no closure */
    },
    {"sourcePopulation", (getter)MigrationNode_source_population, NULL,
     "The source population of the migration event (backwards in time).",
     NULL /* no closure */
    },
    {"destinationPopulation", (getter)MigrationNode_destination_population, NULL,
     "The destination population of the migration event (backwards in time).",
     NULL /* no closure */
    },
    {0}				// sentinel
};

static PyTypeObject MigrationNodeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.MigrationNode",	/*tp_name*/
    sizeof(MigrationNodeObject), /*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Node_dealloc,	/*tp_dealloc*/
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
    "A migration event node in the ARG.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    MigrationNode_getseters, // tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Node_new,			// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};







static PyTypeObject *
node_type(const core::Node *core_node)
{
    using namespace core;
    if (dynamic_cast<const LeafNode*>(core_node)) 
	return &LeafNodeType;
    if (dynamic_cast<const CoalescentNode*>(core_node)) 
	return &CoalescentNodeType;
    if (dynamic_cast<const RecombinationNode*>(core_node)) 
	return &RecombinationNodeType;
    if (dynamic_cast<const GeneConversionNode*>(core_node)) 
	return &GeneConversionNodeType;
    if (dynamic_cast<const MigrationNode*>(core_node)) 
	return &MigrationNodeType;
    assert(false);
    return 0;
}

PyObject *
wrap_node(const core::Node *node, PyObject *py_arg)
{
    NodeObject *py_node = (NodeObject*)_PyObject_New(node_type(node));
    if (!py_node) return 0;

    py_node->py_arg = py_arg; Py_INCREF(py_arg);
    py_node->core_node = node;

    return (PyObject*)py_node;
}

void 
init_nodes(PyObject *module)
{
    if (PyType_Ready(&NodeType) < 0) return;
    Py_INCREF(&NodeType);

    LeafNodeType.tp_base = &NodeType;
    CoalescentNodeType.tp_base = &NodeType;
    RecombinationNodeType.tp_base = &NodeType;
    GeneConversionNodeType.tp_base = &NodeType;
    MigrationNodeType.tp_base = &NodeType;
    
    if (PyType_Ready(&LeafNodeType) < 0) return;
    Py_INCREF(&LeafNodeType);
    if (PyType_Ready(&CoalescentNodeType) < 0) return;
    Py_INCREF(&CoalescentNodeType);
    if (PyType_Ready(&RecombinationNodeType) < 0) return;
    Py_INCREF(&RecombinationNodeType);
    if (PyType_Ready(&GeneConversionNodeType) < 0) return;
    Py_INCREF(&GeneConversionNodeType);
    if (PyType_Ready(&MigrationNodeType) < 0) return;
    Py_INCREF(&MigrationNodeType);

    PyModule_AddObject(module, "LeafNode", (PyObject *)&LeafNodeType);
    PyModule_AddObject(module, "CoalescentNode", (PyObject *)&CoalescentNodeType);
    PyModule_AddObject(module, "RecombinationNode",
		       (PyObject *)&RecombinationNodeType);
    PyModule_AddObject(module, "GeneConversionNode",
		       (PyObject *)&GeneConversionNodeType);
    PyModule_AddObject(module, "MigrationNode", (PyObject *)&MigrationNodeType);
}
