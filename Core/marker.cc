/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "marker.hh"

Marker::Marker(const Marker &other)
    : i_position(other.i_position), i_values(other.i_values)
{
}

Marker::~Marker() 
{
}

