/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "exceptions.hh"

SCM
ex_catch_function(void *data)
{
    ExCatchData *cdata = (ExCatchData*)data;
    return scm_apply(cdata->f, cdata->arg_list, SCM_EOL);
}

SCM
ex_handle_function(void *data, SCM tag, SCM throw_args)
{
    ExHandleData *hdata = (ExHandleData*)data;
    hdata->was_called = true;
    hdata->tag = tag;
    hdata->throw_args = throw_args;
    return SCM_EOL;
}
