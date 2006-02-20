/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__SIMULATE_HH_INCLUDED
#define PYTHON__SIMULATE_HH_INCLUDED

// must be included before anything else...
#ifndef PYTHON_H_INCLUDED
# include <Python.h>
# define PYTHON_H_INCLUDED
#endif
#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif


void init_simulate(PyObject *module);
PyObject *simulate(PyObject *self, PyObject *args, PyObject *kwds);


#endif // PYTHON__SIMULATE_HH_INCLUDED
