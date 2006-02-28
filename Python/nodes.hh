/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__NODES_HH_INCLUDED
#define PYTHON__NODES_HH_INCLUDED

// must be included before anything else...
#ifndef PYTHON_H_INCLUDED
# include <Python.h>
# define PYTHON_H_INCLUDED
#endif

namespace core {
    class Node;
};

// matching C++ class hierarchy here...
struct NodeObject {
    PyObject_HEAD
    const core::Node *core_node;
    PyObject *py_arg;
};
struct LeafNodeObject           { NodeObject base; };
struct CoalescentNodeObject     { NodeObject base; };
struct RecombinationNodeObject  { NodeObject base; };
struct GeneConversionNodeObject { NodeObject base; };
struct MigrationNodeObject      { NodeObject base; };

void 
init_nodes(PyObject *module);
PyObject *
wrap_node(const core::Node *node, PyObject *py_arg);

#endif // PYTHON__NODES_HH_INCLUDED
