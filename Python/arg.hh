/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__ARG_HH_INCLUDED
#define PYTHON__ARG_HH_INCLUDED

// must be included before anything else...
#ifndef PYTHON_H_INCLUDED
# include <Python.h>
# define PYTHON_H_INCLUDED
#endif

namespace core {
    class ARG;
};

struct ARGObject {
    PyObject_HEAD
    core::ARG *arg;
};

void init_arg(PyObject *module);
PyObject *wrap_arg(core::ARG *arg);


#endif // PYTHON__ARG_HH_INCLUDED
