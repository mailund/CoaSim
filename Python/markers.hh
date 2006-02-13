/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__MARKERS_HH_INCLUDED
#define PYTHON__MARKERS_HH_INCLUDED

// must be included before anything else...
#ifndef PYTHON_H_INCLUDED
# include <Python.h>
# define PYTHON_H_INCLUDED
#endif

namespace core {
    class Marker;
}

// This is a bit ugly, but it emulates the C++ marker hierarchy here
struct MarkerObject               { PyObject_HEAD core::Marker *core_marker; };
struct SNPMarkerObject            { MarkerObject base; };
struct TraitMarkerObject          { MarkerObject base; };
struct MicroSatelliteMarkerObject { MarkerObject base; };
struct PythonMarkerObject         { MarkerObject base; };
extern PyTypeObject MarkerType;
extern PyTypeObject SNPMarkerType;
extern PyTypeObject TraitMarkerType;
extern PyTypeObject MicroSatelliteMarkerType;
extern PyTypeObject PythonMarkerType;

void init_markers(PyObject *module);

#endif
