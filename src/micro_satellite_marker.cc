
#include "micro_satellite_marker.hh"

#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif

namespace
{
  class MicroSatelliteMutator : public Mutator {
  public:
    MicroSatelliteMutator(const MicroSatelliteMarker &marker);
    bool edge_has_mutation(double parent_time, double child_time);
    int  mutate_to(int current_value);

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

  int MicroSatelliteMutator::mutate_to(int current_value)
  {
    using namespace Distribution_functions;
    int new_value = _marker.value(irand(_marker.size()-1));
    if (new_value == current_value)
      new_value = _marker.value(_marker.size()-1);
    return new_value;
  }
}

Mutator *MicroSatelliteMarker::create_mutator(const RetiredInterval &ri) const
{
  return new MicroSatelliteMutator(*this);
}
