
#include "snp_marker.hh"

#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef RETIRED_INTERVAL_HH_INCLUDED
# include "retired_interval.hh"
#endif

namespace
{
  class SNPMutator : public Mutator {
    double _mutation_point;	// the point on the "surface" where
				// the mutation sits
    double _surface_so_far;	// the surface seen so far
  public:
    SNPMutator(const RetiredInterval &ri);
    bool edge_has_mutation(double parent_time, double child_time);
    int  mutate_to(int current_value);
  };

  SNPMutator::SNPMutator(const RetiredInterval &ri)
    : _surface_so_far(0.0)
  {
    _mutation_point = ri.surface() * Distribution_functions::uniform();
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

  int SNPMutator::mutate_to(int current_value)
  {
    return !current_value;
  }
}

Mutator *SNPMarker::create_mutator(const RetiredInterval &ri) const
{
  return new SNPMutator(ri);
}
