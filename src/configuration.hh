
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
    struct uninitialized_marker_type : public std::logic_error {
      uninitialized_marker_type() : std::logic_error("uninitialized marker."){}
    };

    // exception thrown if we try to add a value to a value set that
    // doesn't fit the type of the value set
    struct illegal_value : public std::logic_error {
      illegal_value() : std::logic_error("illegal marker value.") {}
    };

    virtual ~ValueSet() {};

    size_t size() const { return _values.size(); }

    int value(size_t index)     const throw(std::out_of_range);
    virtual void add_value(int value) throw(uninitialized_marker_type,
					    illegal_value) = 0;
  protected:
    std::vector<int> _values;
  };

  // exception thrown if the configuration is initialized with
  // un-sorted positions
  struct out_of_sequence : public std::logic_error {
    out_of_sequence() : std::logic_error("Marker positions not sorted."){}
  };

  // initialize the configuration with the marker positions given by
  // the sequence from begin to end -- an exception is thrown if the
  // positions are not sorted in increasing order
  template <typename InItr>
  Configuration(double rho, double Q, double G, double growth,
		InItr begin, InItr end) throw(out_of_sequence);
  ~Configuration();

  // number of markers for the configuration
  size_t no_markers() const { return _positions.size(); }

  // the positions of the markers
  double position(size_t index) const throw(std::out_of_range);


  // accessors to the possible values of a marker
  ValueSet &value_set(size_t marker) const
    throw(std::out_of_range);

  enum marker_t { MT_SNP, MT_MICROSATELLITE, MT_TRAIT, };
  void set_marker_type(size_t marker, marker_t type) throw(std::out_of_range);


  double rho()    const { return _rho; }
  double Q()      const { return _Q; }
  double G()      const { return _G; }
  double growth() const { return _growth; }

private:
  // disable these
  Configuration(const Configuration&);
  Configuration& operator = (const Configuration&);

  // set the value sets to uninitialized values
  void initialize_value_sets();


  std::vector<double>    _positions;
  std::vector<ValueSet*> _value_sets;

  double _rho;
  double _Q;
  double _G;
  double _growth;
};


template <typename InItr>
Configuration::Configuration(double rho, double Q, double G, double growth,
			     InItr begin, InItr end)
  throw(out_of_sequence)
  : _rho(rho), _Q(Q), _G(G), _growth(growth)
{
  for (InItr itr = begin; itr != end; ++itr)
    _positions.push_back(*itr);
  for (size_t m = 1; m < _positions.size(); ++m)
    if (_positions[m-1] >= _positions[m]) throw out_of_sequence();
  initialize_value_sets();
}



inline double Configuration::position(size_t index)
  const throw(std::out_of_range)
{
  return _positions.at(index);
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
