/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__SIMULATE_HH_INCLUDED
#define PYTHON__SIMULATE_HH_INCLUDED

// must be included before anything else...
#include <Python.h>
#include <stdexcept>


void init_simulate(PyObject *module);
PyObject *simulate(PyObject *self, PyObject *args, PyObject *kwds);


#endif // PYTHON__SIMULATE_HH_INCLUDED
