/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "micro_satellite_marker.hh"

#ifndef CORE__DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include "node.hh"
#endif


using namespace core;

Marker *
core::MicroSatelliteMarker::copy() const
{
    return new MicroSatelliteMarker(*this);
}

bool
core::MicroSatelliteMarker::run_first() const
{
    return false;
}

int
core::MicroSatelliteMarker::default_value() const 
{
    if (size() == 0) throw std::out_of_range("No value set");
    return i_values.front(); 
}


namespace
{
  class MicroSatelliteMutator : public Mutator {
  public:
    MicroSatelliteMutator(const MicroSatelliteMarker &marker);
    bool edge_has_mutation(double parent_time, double child_time);
    int  mutate_to(const Node &n, unsigned int marker_index)
      throw (retry_mutation, retry_arg);

  private:
    const MicroSatelliteMarker &_marker;
  };

  MicroSatelliteMutator::MicroSatelliteMutator(const MicroSatelliteMarker &marker)
    : _marker(marker)
  {
  }

  bool MicroSatelliteMutator::edge_has_mutation(double parent_time, 
						double child_time) 
  {
    using namespace Distribution_functions;
    double time = parent_time - child_time;
    return uniform() < expdist(_marker.mu(),time);
  }

  int MicroSatelliteMutator::mutate_to(const Node &n, 
				       unsigned int marker_index)
    throw (retry_mutation, retry_arg) 
  {
    using namespace Distribution_functions;
    int current_value = n.state(marker_index);
    int new_value = _marker.value(irand(_marker.size()-1));
    if (new_value == current_value)
      new_value = _marker.value(_marker.size()-1);
    return new_value;
  }
}

Mutator *
core::MicroSatelliteMarker::create_mutator(const Configuration   &conf,
					   const RetiredInterval &ri) const
{

  return new MicroSatelliteMutator(*this);
}


const char *
core::MicroSatelliteMarker::type() const
{
    return "ms";
}
