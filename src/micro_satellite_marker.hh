
#ifndef MICRO_SATELLITE_MARKER_HH_INCLUDED
#define MICRO_SATELLITE_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class MicroSatelliteMarker : public Marker
{
public:
  MicroSatelliteMarker(double mu) : Marker(), _mu(mu) {}

  virtual int default_value() const {
    if (size() == 0) throw std::out_of_range("No value set");
    return _values.front(); 
  }

  virtual void add_value(int value) throw(illegal_value)
  {
    if (value < 0) throw illegal_value();
    _values.push_back(value);
  }

  virtual Mutator *create_mutator(const RetiredInterval &ri) const;
  double mu() const { return _mu; }

private:
  double _mu; // mutation parameter
};


#endif
