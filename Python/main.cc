/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__MARKERS_HH_INCLUDED
# include "markers.hh"
#endif
#ifndef PYTHON__ARG_HH_INCLUDED
# include "arg.hh"
#endif
#ifndef PYTHON__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif
#ifndef PYTHON__TREES_HH_INCLUDED
# include "trees.hh"
#endif
#ifndef PYTHON__NODES_HH_INCLUDED
# include "nodes.hh"
#endif
#ifndef PYTHON__SIMULATE_HH_INCLUDED
# include "simulate.hh"
#endif

static PyMethodDef coasim_methods[] = {
    {"simulate",
     (PyCFunction)simulate,
     METH_VARARGS | METH_KEYWORDS,
     "Simulate an ancestral recombination graph." },

    {NULL}  /* Sentinel */
};

char *doc_string = \
"CoaSim/Python -- Python bindings for Coasim\n" \
"Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>\n" \
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
//initCoaSim(void) 
initCore(void) 
{
    PyObject* m = Py_InitModule3("Core", coasim_methods, doc_string);

    init_markers(m);
    init_arg(m);
    init_intervals(m);
    init_trees(m);
    init_nodes(m);
    init_simulate(m);
}
