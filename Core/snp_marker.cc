/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "snp_marker.hh"

#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef RETIRED_INTERVAL_HH_INCLUDED
# include "retired_interval.hh"
#endif
#ifndef NODE_HH_INCLUDED
# include "node.hh"
#endif


Marker *
SNPMarker::copy() const
{
    return new SNPMarker(*this);
}

bool
SNPMarker::run_first() const
{
    return false;
}

int
SNPMarker::default_value() const
{
    return 0;
}

namespace
{
    class SNPMutator : public Mutator {
	const Configuration &_conf;
    
	// allowed frequencies, translated into leaf node counts
	unsigned int _low_leaf_count;
	unsigned int _high_leaf_count;

	double _mutation_point;	// the point on the "surface" where
				// the mutation sits
	double _surface_so_far;	// the surface seen so far

    public:
	SNPMutator(const Configuration &conf,
		   unsigned int low_leaf_count,
		   unsigned int high_leaf_count,
		   double mutation_point);

	bool edge_has_mutation(double parent_time, double child_time);
	int  mutate_to(const Node &n, unsigned int marker_index);
    };

    SNPMutator::SNPMutator(const Configuration &conf,
			   unsigned int low_leaf_count, 
			   unsigned int high_leaf_count,
			   double mutation_point)
	: _conf(conf),
	  _low_leaf_count(low_leaf_count), _high_leaf_count(high_leaf_count),
	  _mutation_point(mutation_point), _surface_so_far(0.0)
    {
    }

    bool SNPMutator::edge_has_mutation(double parent_time, double child_time) 
    {
	double edge_length = parent_time - child_time;
	bool mutate = (_surface_so_far <= _mutation_point
		       and
		       _mutation_point < _surface_so_far+edge_length);
	_surface_so_far += edge_length;
	return mutate;
    }

    int SNPMutator::mutate_to(const Node &n, unsigned int marker_index)
    {
	// check frequency
	unsigned int leaf_count = n.leaves_at_point(_conf.position(marker_index));
	if ((leaf_count < _low_leaf_count) or (_high_leaf_count < leaf_count))
	    throw Mutator::retry_mutation();
	return !n.state(marker_index);
    }
}

static inline void swap(unsigned int &i, unsigned int &j)
{ unsigned int tmp = i; i = j; j = tmp; }

Mutator *SNPMarker::create_mutator(const Configuration   &conf,
				   const RetiredInterval &ri) const
{
    unsigned int low_leaf_count
	= static_cast<unsigned int>(ceil(i_low_freq*conf.no_leaves()));
    unsigned int high_leaf_count
	= static_cast<unsigned int>(floor(i_high_freq*conf.no_leaves()));

    // possible due to ceil/floor
    if (high_leaf_count < low_leaf_count) swap(low_leaf_count,high_leaf_count);

    double mutation_point = ri.surface() * Distribution_functions::uniform();

    return new SNPMutator(conf, low_leaf_count, high_leaf_count, mutation_point);
}

void 
SNPMarker::to_text(std::ostream &os) const
{
    os << "snp";
}

