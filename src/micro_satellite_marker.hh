
#ifndef MICRO_SATELLITE_MARKER_HH_INCLUDED
#define MICRO_SATELLITE_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class MicroSatelliteMarker : public Marker
{
public:
  MicroSatelliteMarker(double mu) : Marker(), i_mu(mu) {}

  virtual int default_value() const {
    if (size() == 0) throw std::out_of_range("No value set");
    return i_values.front(); 
  }

  virtual void add_value(int value) throw(illegal_value)
  {
    if (value < 0) throw illegal_value();
    i_values.push_back(value);
  }

  virtual Mutator *create_mutator(const Configuration &conf,
				  const RetiredInterval &ri) const;
  double mu() const { return i_mu; }

private:
  double i_mu; // mutation parameter
};


#endif
