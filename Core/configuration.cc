/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "configuration.hh"

Configuration::~Configuration() 
{
    for (int i = 0; i < no_markers(); ++i)
	{
	    delete i_first_markers[i];
	    delete i_plain_markers[i];
	}
    delete[] i_first_markers; 
    delete[] i_plain_markers; 
}
