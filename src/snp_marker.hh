
#ifndef SNP_MARKER_HH_INCLUDED
#define SNP_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class SNPMarker : public Marker
{
public:
  SNPMarker() : Marker()
  { _values.push_back(0); _values.push_back(1); }

  virtual int default_value() const { return 0; }

  virtual void add_value(int value) throw(illegal_value)
  { throw illegal_value(); } // don't add to SNP markers

  virtual Mutator *create_mutator(const RetiredInterval &ri) const;
};


#endif
