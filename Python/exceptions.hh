/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim/Python -- Python bindings for Coasim
 *
 *  Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>
 */

#ifndef PYTHON__EXCEPTIONS_HH_INCLUDED
#define PYTHON__EXCEPTIONS_HH_INCLUDED

#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif

// throw this one to propagate a python exception to outside the
// simulation -- remember to set the correct python exception before
// pulling this handle!
struct PyException : std::exception {};


#endif // PYTHON__EXCEPTIONS_HH_INCLUDED
