/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "trait_marker.hh"

#ifndef CORE__DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef CORE__RETIRED_INTERVAL_HH_INCLUDED
# include "retired_interval.hh"
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif

using namespace core;

Marker *
core::TraitMarker::copy() const
{
    return new TraitMarker(*this);
}

bool
core::TraitMarker::run_first() const
{
    return true;
}

int
core::TraitMarker::default_value() const
{
    return 0;
}

namespace
{
    class TraitMutator : public Mutator {
	const Configuration &_conf;

	// allowed frequencies, translated into leaf node counts
	unsigned int _low_leaf_count;
	unsigned int _high_leaf_count;

	double _mutation_point;	// the point on the "surface" where
				// the mutation sits
	double _surface_so_far;	// the surface seen so far
    public:
	TraitMutator(const Configuration &conf,
		     unsigned int low_leaf_count,
		     unsigned int high_leaf_count,
		     double mutation_point);
	bool edge_has_mutation(double parent_time, double child_time);
	int  mutate_to(const Node &n, unsigned int marker_index);
    };

    TraitMutator::TraitMutator(const Configuration &conf,
			       unsigned int low_leaf_count, 
			       unsigned int high_leaf_count,
			       double mutation_point)
	: _conf(conf),
	  _low_leaf_count(low_leaf_count), _high_leaf_count(high_leaf_count),
	  _mutation_point(mutation_point), _surface_so_far(0.0)
    {
    }

    bool TraitMutator::edge_has_mutation(double parent_time, double child_time) 
    {
	double edge_length = parent_time - child_time;
	bool mutate = (_surface_so_far <= _mutation_point
		       and
		       _mutation_point < _surface_so_far+edge_length);
	_surface_so_far += edge_length;
	return mutate;
    }

    int TraitMutator::mutate_to(const Node &n, unsigned int marker_index)
    {
	// check frequency
	unsigned int leaf_count = n.leaves_at_point(_conf.position(marker_index));
	if ((leaf_count < _low_leaf_count) or (_high_leaf_count < leaf_count))
	    throw Mutator::retry_arg();

	return !n.state(marker_index);
    }
}

static inline void swap(unsigned int &i, unsigned int &j)
{ unsigned int tmp = i; i = j; j = tmp; }

Mutator *TraitMarker::create_mutator(const Configuration   &conf,
				     const RetiredInterval &ri) const
{
    unsigned int low_leaf_count
	= static_cast<unsigned int>(ceil(i_low_freq*conf.no_leaves()));
    unsigned int high_leaf_count
	= static_cast<unsigned int>(floor(i_high_freq*conf.no_leaves()));

    // possible due to ceil/floor
    if (high_leaf_count < low_leaf_count) swap(low_leaf_count,high_leaf_count);

    double mutation_point = ri.surface() * Distribution_functions::uniform();

    return new TraitMutator(conf,low_leaf_count,high_leaf_count,mutation_point);
}

const char *
core::TraitMarker::type() const
{
    return "trait";
}
