/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef GUILE__EXCEPTIONS_HH_INCLUDED
#define GUILE__EXCEPTIONS_HH_INCLUDED

#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif
#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif


extern "C" {
    struct ExCatchData {
	SCM f;
	SCM arg_list;
	ExCatchData(SCM f, SCM arg_list) : f(f), arg_list(arg_list) {}
    };
    struct ExHandleData {
	bool was_called;
	SCM tag;
	SCM throw_args;
	ExHandleData() : was_called(false) {}
    };

    SCM ex_catch_function(void *data);
    SCM ex_handle_function(void *data, SCM tag, SCM throw_args);
}

namespace guile {

    struct SchemeException : public std::exception {
	SCM tag;
	SCM throw_args;
	SchemeException(SCM tag, SCM throw_args)
	    : tag(tag), throw_args(throw_args) {}

	virtual const char* what() const throw() { return "SchemeException"; }
    };

    inline void rethrow_if_failed(ExHandleData &hdata)
    {
	if (hdata.was_called)
	    throw SchemeException(hdata.tag, hdata.throw_args);
    }

    inline void propagate(SchemeException &sex)
    {
	scm_throw(sex.tag, sex.throw_args);
    }

    inline SCM wrapped_apply(SCM f, SCM args)
    {
	ExCatchData cdata(f, args);
	ExHandleData hdata;
	SCM return_val = scm_internal_catch(SCM_BOOL_T,
					    ex_catch_function, &cdata,
					    ex_handle_function, &hdata);
	rethrow_if_failed(hdata);
	return return_val;
    }
}


#endif
