
#include "micro_satellite_marker.hh"

namespace
{
  class MicroSatelliteMutator : public Mutator {
    double _mutation_point;
  public:
    MicroSatelliteMutator();
    bool edge_has_mutation(double parent_time, double child_time);
    int  mutate_to(int current_value);
  };

  MicroSatelliteMutator::MicroSatelliteMutator()
  {
  }

  bool MicroSatelliteMutator::edge_has_mutation(double parent_time, 
						double child_time) 
  {
    return false;
  }

  int MicroSatelliteMutator::mutate_to(int current_value)
  {
    return current_value;
  }
}

Mutator *MicroSatelliteMarker::create_mutator(const RetiredInterval &ri) const
{
  return new MicroSatelliteMutator();
}
