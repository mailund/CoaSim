
#include "configuration.hh"


namespace
{

  class Uninitialized : public Configuration::ValueSet
  {
  public:
    virtual void add_value(int value)
      throw(uninitialized_marker_type,illegal_value);
  };

  class SNPSet : public Configuration::ValueSet
  {
  public:
    SNPSet()
    { _values.push_back(0); _values.push_back(1); }
      
    virtual void add_value(int value)
      throw(uninitialized_marker_type,illegal_value);
  };

  class MicroSatelliteSet : public Configuration::ValueSet
  {
  public:
    virtual void add_value(int value)
      throw(uninitialized_marker_type,illegal_value);
  };

  class TraitSet : public Configuration::ValueSet
  {
  public:
    TraitSet()
    { _values.push_back(0); _values.push_back(1); }

    virtual void add_value(int value)
      throw(uninitialized_marker_type,illegal_value);
  };
}

void Uninitialized::add_value(int value)
  throw(uninitialized_marker_type,illegal_value)
{
  throw uninitialized_marker_type("Accessing an uninitialized marker.");
}

void SNPSet::add_value(int value)
  throw(uninitialized_marker_type,illegal_value)
{
  throw illegal_value("Setting illegal marker type.");
}

void MicroSatelliteSet::add_value(int value)
  throw(uninitialized_marker_type,illegal_value)
{
  if (value < 0) throw illegal_value("Setting illegal marker type.");
  _values.push_back(value);
}

void TraitSet::add_value(int value)
  throw(uninitialized_marker_type,illegal_value)
{
  throw illegal_value("Setting illegal marker type.");
}

void Configuration::initialize_value_sets()
{
  for (size_t i = 0; i < _positions.size(); ++i)
    _value_sets.push_back(new Uninitialized());
}


Configuration::~Configuration()
{
  typedef std::vector<ValueSet*>::iterator itr_t;
  for (itr_t i = _value_sets.begin(); i != _value_sets.end(); ++i)
    delete *i;
  _value_sets.clear();
}


void Configuration::set_marker_type(size_t marker, marker_t type)
  throw(std::out_of_range)
{
  delete _value_sets.at(marker);
  switch(type)
    {
    case MT_SNP:
      _value_sets.at(marker) = new SNPSet();
      break;
    case MT_MICROSATELLITE:
      _value_sets.at(marker) = new MicroSatelliteSet();
      break;
    case MT_TRAIT:
      _value_sets.at(marker) = new TraitSet();
      break;
    }
}
