
#ifndef SNP_MARKER_HH_INCLUDED
#define SNP_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class SNPMarker : public Marker
{
public:
  SNPMarker(double low_freq, double high_freq) 
    : Marker(), _low_freq(low_freq), _high_freq(high_freq)
  { _values.push_back(0); _values.push_back(1); }

  virtual int default_value() const { return 0; }

  virtual void add_value(int value) throw(illegal_value)
  { throw illegal_value(); } // don't add to SNP markers

  virtual Mutator *create_mutator(const Configuration &conf,
				  const RetiredInterval &ri) const;

  double low_freq()  const { return _low_freq; }
  double high_freq() const { return _high_freq; }

private:
  double _low_freq, _high_freq; // allowed range of mutation frequencies
};


#endif
