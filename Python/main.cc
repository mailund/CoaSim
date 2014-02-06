/*
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006, 2014 by Thomas Mailund <mailund@mailund.dk>
 */

#include "markers.hh"
#include "arg.hh"
#include "intervals.hh"
#include "trees.hh"
#include "nodes.hh"
#include "simulate.hh"

static PyMethodDef coasim_methods[] = {
    {"simulate",
     (PyCFunction)simulate,
     METH_VARARGS | METH_KEYWORDS,
     "Simulate an ancestral recombination graph." },

    {NULL}  /* Sentinel */
};

char *doc_string = (char*)\
"CoaSim/Python -- Python bindings for Coasim\n" \
"Copyright (C) 2006, 2014 by Thomas Mailund <mailund@birc.au.dk>\n" \
"\n" \
"This is the Core module, binding the Python modules to the C++\n" \
"simulation core." \
"\n" \
"Please see http://www.daimi.au.dk/~mailund/CoaSim for more details\n" \
"on the CoaSim simulator.\n";

#ifndef PyMODINIT_FUNC
#define PyMODINIT_FUNC void
#endif
PyMODINIT_FUNC
initCoaSimCore(void)
{
    PyObject* m = Py_InitModule3("CoaSimCore", coasim_methods, doc_string);

    init_markers(m);
    init_arg(m);
    init_intervals(m);
    init_trees(m);
    init_nodes(m);
    init_simulate(m);
}
