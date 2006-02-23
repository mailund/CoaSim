/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif
#ifndef PYTHON__TREES_HH_INCLUDED
# include "trees.hh"
#endif

#ifndef CORE__RETIRED_INTERVAL_HH_INCLUDED
# include <Core/retired_interval.hh>
#endif

#ifndef SSTREAM_INCLUDED
# include <sstream>
# define SSTREAM_INCLUDED
#endif

static PyObject *
start(IntervalObject *self)
{
    return Py_BuildValue("d",self->interval->start());
}

static PyObject *
end(IntervalObject *self)
{
    return Py_BuildValue("d",self->interval->end());
}

static PyObject *
length(IntervalObject *self)
{
    return Py_BuildValue("d",self->interval->length());
}

static PyObject *
tree(IntervalObject *self)
{
    return wrap_tree(self->interval, self->py_arg);
}

static PyGetSetDef Interval_getseters[] = {
    {"start", (getter)start, NULL,
     "Start point of the interval.",
     NULL /* no closure */
    },
    {"end", (getter)end, NULL,
     "End point of the interval.",
     NULL /* no closure */
    },
    {"length", (getter)length, NULL,
     "Length of the interval.",
     NULL /* no closure */
    },
    {"tree", (getter)tree, NULL,
     "The genealogy of the interval.",
     NULL /* no closure */
    },

    {0}				// sentinel
};

static PyObject *
Interval_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    IntervalObject *self = (IntervalObject *)type->tp_alloc(type, 0);
    self->py_arg = 0;
    self->interval = 0;
    return (PyObject*)self;
}

static int
Interval_clear(IntervalObject *self)
{
    Py_XDECREF(self->py_arg);
    self->py_arg = 0;
    self->interval = 0;
    return 0;
}

static void
Interval_dealloc(IntervalObject *self)
{
    Interval_clear(self);
    self->ob_type->tp_free((PyObject*)self);
}

static int
Interval_compare(IntervalObject *i1, IntervalObject *i2)
{
    if (i1->interval == i2->interval) return 0;
    return -1;
}

static PyObject *
Interval_str(IntervalObject *i)
{
    std::ostringstream os; 
    os << '[' << i->interval->start() << ", " << i->interval->end() << ')';
    return PyString_FromString(os.str().c_str());
}

static int
Interval_print(IntervalObject *i, FILE *fp, int flags)
{
    std::ostringstream os; 
    os << '[' << i->interval->start() << ", " << i->interval->end() << ')';
    fprintf(fp, os.str().c_str());
    return 0;
}


static PyTypeObject IntervalType = {
    PyObject_HEAD_INIT(NULL)
    0,				/*ob_size*/
    "CoaSim.Interval",		/*tp_name*/
    sizeof(IntervalObject),	/*tp_basicsize*/
    0,				/*tp_itemsize*/
    (destructor)Interval_dealloc, /*tp_dealloc*/
    (printfunc)Interval_print,	/*tp_print*/
    0,				/*tp_getattr*/
    0,				/*tp_setattr*/
    (cmpfunc)Interval_compare,	/*tp_compare*/
    0,				/*tp_repr*/
    0,				/*tp_as_number*/
    0,				/*tp_as_sequence*/
    0,				/*tp_as_mapping*/
    0,				/*tp_hash */
    0,				/*tp_call*/
    (reprfunc)Interval_str,	/*tp_str*/
    0,				/*tp_getattro*/
    0,				/*tp_setattro*/
    0,				/*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
    "Interval sharing the same genealogy.", /* tp_doc */
    0,				/* tp_traverse */
    0,				/* tp_clear */
    0, 				/* tp_richcompare */
    0,				// tp_weaklistoffset
    0,				// tp_iter
    0,				// tp_iternext
    0,				// tp_methods
    0,				// tp_members
    Interval_getseters,		// tp_getset
    0, 				// tp_base
    0,				// tp_dict
    0,				// tp_descr_get
    0,				// tp_descr_set
    0,				// tp_dictoffset
    0,				// tp_init
    0,				// tp_alloc
    Interval_new,		// tp_new
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
wrap_interval(const core::RetiredInterval *interval, ARGObject *py_arg)
{
    IntervalObject *py_interval 
      = (IntervalObject*)_PyObject_New((PyTypeObject*)&IntervalType);
    if (!py_interval) return 0;

    py_interval->py_arg = py_arg; Py_INCREF(py_arg);
    py_interval->interval = interval;

    return (PyObject*)py_interval;
}

void 
init_intervals(PyObject *module)
{
    if (PyType_Ready(&IntervalType) < 0) return;
    Py_INCREF(&IntervalType);

}
