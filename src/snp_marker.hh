
#ifndef SNP_MARKER_HH_INCLUDED
#define SNP_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class SNPMarker : public Marker
{
public:
  SNPMarker(double low_freq, double high_freq) 
    : Marker(), i_low_freq(low_freq), i_high_freq(high_freq)
  { i_values.push_back(0); i_values.push_back(1); }

  virtual int default_value() const { return 0; }

  virtual void add_value(int value) throw(illegal_value)
  { throw illegal_value(); } // don't add to SNP markers

  virtual Mutator *create_mutator(const Configuration &conf,
				  const RetiredInterval &ri) const;

  double low_freq()  const { return i_low_freq; }
  double high_freq() const { return i_high_freq; }

private:
  double i_low_freq, i_high_freq; // allowed range of mutation frequencies
};


#endif
