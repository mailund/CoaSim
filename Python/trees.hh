/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__TREES_HH_INCLUDED
#define PYTHON__TREES_HH_INCLUDED

// must be included before anything else...
#ifndef PYTHON_H_INCLUDED
# include <Python.h>
# define PYTHON_H_INCLUDED
#endif

#ifndef PYTHON__ARG_HH_INCLUDED
# include "arg.hh"
#endif

namespace core {
    class RetiredInterval;
};

struct TreeObject {
    PyObject_HEAD
    ARGObject *py_arg;
    // A tree is just represented by its interval -- we only use two
    // different types since we want different methods...
    const core::RetiredInterval *interval;
};

void 
init_trees(PyObject *module);

PyObject *
wrap_tree(const core::RetiredInterval *interval, ARGObject *py_arg);


#endif // PYTHON__TREES_HH_INCLUDED
