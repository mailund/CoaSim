/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef CORE__BUILDER_HH_INCLUDED
#define CORE__BUILDER_HH_INCLUDED

#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif

namespace core {

    class Builder
    {
    public:
	Builder(const Configuration &conf) : i_conf(conf) {};
	~Builder() {};

	// Builds an ARG.  The ARG is dynamically allocated and must be
	// deleted after use.
	ARG *build() const;

    private:
	const Configuration &i_conf;
    };


}

#endif
