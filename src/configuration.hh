
#ifndef CONFIGURATION_HH
#define CONFIGURATION_HH

#include <vector>
#include <stdexcept>

class Configuration
{
public:
  // abstract class for the different possible value sets
  class ValueSet
  {
  public:
    // exception thrown if we try to add to a value set before the type
    // of the marker has been initialized
    class uninitialized_marker_type : public std::exception {};

    // exception thrown if we try to add a value to a value set that
    // doesn't fit the type of the value set
    class illegal_value : public std::exception {};

    virtual ~ValueSet() {};

    size_t size() const { return _values.size(); }

    int value(size_t index)     const throw(std::out_of_range);
    virtual void add_value(int value) throw(uninitialized_marker_type,
					    illegal_value) = 0;
  protected:
    std::vector<int> _values;
  };

  Configuration(size_t no_markers);
  ~Configuration();

  // number of markers for the configuration
  size_t no_markers() const { return _positions.size(); }

  // the positions of the markers
  double position(size_t index)                 const throw(std::out_of_range);
  void set_position(size_t index, double pos)         throw(std::out_of_range);


  // accessors to the possible values of a marker
  ValueSet &value_set(size_t marker) const
    throw(std::out_of_range);

  enum marker_t { MT_SNP, MT_MICROSATELLITE, MT_TRAIT, };
  void set_marker_type(size_t marker, marker_t type) throw(std::out_of_range);

private:
  std::vector<double>    _positions;
  std::vector<ValueSet*> _value_sets;
};



inline double Configuration::position(size_t index)
  const throw(std::out_of_range)
{
  return _positions.at(index);
}

inline void Configuration::set_position(size_t index, double pos)
  throw(std::out_of_range)
{
  if (pos < 0.0 or 1.0 <= pos)
    throw std::out_of_range("position must be in [0.0,1.0)");
  _positions.at(index) = pos;
}


inline int Configuration::ValueSet::value(size_t index)
  const throw (std::out_of_range)
{
  return _values.at(index);
}

inline Configuration::ValueSet &Configuration::value_set(size_t marker)
  const throw(std::out_of_range)
{
  return *_value_sets.at(marker);
}


#endif // CONFIGURATION_HH
