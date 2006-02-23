/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__TREES_HH_INCLUDED
# include "trees.hh"
#endif
#ifndef PYTHON__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif
#ifndef PYTHON__NODES_HH_INCLUDED
# include "nodes.hh"
#endif

#ifndef CORE__RETIRED_INTERVAL_HH_INCLUDED
# include <Core/retired_interval.hh>
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif

#ifndef SSTREAM_INCLUDED
# include <sstream>
# define SSTREAM_INCLUDED
#endif


static PyObject *
interval(TreeObject *self)
{
    return wrap_interval(self->interval, self->py_arg);
}

static PyObject *
branch_length(TreeObject *self)
{
    return Py_BuildValue("d", self->interval->surface());
}

static PyObject *
heigth(TreeObject *self)
{
    return Py_BuildValue("d", self->interval->top_node()->time());
}

static PyObject *
root(TreeObject *self)
{
    return wrap_node(self->interval->top_node(), (PyObject*)self->py_arg);
}


static PyGetSetDef Tree_getseters[] = {
    {"interval", (getter)interval, NULL,
     "The interval the tree is the local genealogy of.",
     NULL /* no closure */
    },
    {"branchLength", (getter)branch_length, NULL,
     "Total branch length of the local genealogy of.",
     NULL /* no closure */
    },
    {"height", (getter)heigth, NULL,
     "Height of the local genealogy of (time to the local MRCA).",
     NULL /* no closure */
    },
    {"root", (getter)root, NULL,
     "Root of the local genealogy of (the local MRCA).",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyObject *
Tree_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    TreeObject *self = (TreeObject *)type->tp_alloc(type, 0);
    self->py_arg = 0;
    self->interval = 0;
    return (PyObject*)self;
}

static int
Tree_clear(TreeObject *self)
{
    Py_XDECREF(self->py_arg);
    self->py_arg = 0;
    self->interval = 0;
    return 0;
}

static void
Tree_dealloc(TreeObject *self)
{
    Tree_clear(self);
    self->ob_type->tp_free((PyObject*)self);
}

static int
Tree_compare(TreeObject *t1, TreeObject *t2)
{
    if (t1->interval == t2->interval) return 0;
    return -1;
}

static PyObject *
Tree_str(TreeObject *t)
{
    std::ostringstream os; 
    t->interval->top_node()->print_tree_at_point(os, t->interval->start());
    return PyString_FromString(os.str().c_str());
}

static int
Tree_print(TreeObject *t, FILE *fp, int flags)
{
    std::ostringstream os; 
    t->interval->top_node()->print_tree_at_point(os, t->interval->start());
    fprintf(fp, os.str().c_str());
    return 0;
}

static PyTypeObject TreeType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.Tree",		/*tp_name*/
    sizeof(TreeObject),		/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Tree_dealloc, /*tp_dealloc*/
    (printfunc)Tree_print,	/*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    (cmpfunc)Tree_compare,	/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    (reprfunc)Tree_str,		/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "Tree for a local genealogy.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    Tree_getseters,		// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Tree_new,		// tp_new
    0,				// tp_free
    0,				// tp_is_gc
    0,				// tp_bases
    0,				// tp_mro
    0,				// tp_cache
    0,				// tp_subclasses
    0,				// tp_weaklist
    0,				// tp_del
};




PyObject *
wrap_tree(const core::RetiredInterval *interval, ARGObject *py_arg)
{
    TreeObject *py_tree 
      = (TreeObject*)_PyObject_New((PyTypeObject*)&TreeType);
    if (!py_tree) return 0;

    py_tree->py_arg = py_arg; Py_INCREF(py_arg);
    py_tree->interval = interval;

    return (PyObject*)py_tree;
}

void 
init_trees(PyObject *module)
{
    if (PyType_Ready(&TreeType) < 0) return;
    Py_INCREF(&TreeType);

}
