
#ifndef TRAIT_MARKER_HH_INCLUDED
#define TRAIT_MARKER_HH_INCLUDED

#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

class TraitMarker : public Marker
{
public:
  TraitMarker() : Marker()
  { _values.push_back(0); _values.push_back(1); }
  
  virtual void add_value(int value) throw(illegal_value)
  { throw illegal_value(); } // don't add to trait markers
};


#endif
