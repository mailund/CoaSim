
#ifndef CONFIGURATION_HH_INCLUDED
#define CONFIGURATION_HH_INCLUDED

#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif

class Marker;
class SimulationMonitor;

class Configuration
{
public:

  // Exception thrown if the configuration is initialized with
  // un-sorted positions
  struct out_of_sequence : public std::logic_error {
    out_of_sequence() : std::logic_error("Marker positions not sorted."){}
  };

  // Exception thrown if we try to add to a value set before the type
  // of the marker has been initialized
  struct uninitialized_marker : public std::logic_error {
    uninitialized_marker() : std::logic_error("uninitialized marker."){}
  };

  // Initialize the configuration with the marker positions given by
  // the sequence from begin to end -- an exception is thrown if the
  // positions are not sorted in increasing order; the build
  // parameters rho, Q, G, and growth; evolution parameter mu; and a
  // flag specifying if the full output of the simulation is desired,
  // or just information about the leaf nodes.
  template <typename InItr>
  Configuration(unsigned int no_leaves,
		InItr positions_begin, InItr positions_end,
		double rho, double Q, double G, double growth,
		double mu,
		bool print_all_nodes = false,
		SimulationMonitor *monitor = 0)
    throw(out_of_sequence);
  ~Configuration();

  unsigned int no_leaves() const { return _no_leaves; }

  // Number of markers for the configuration
  size_t no_markers() const { return _positions.size(); }
  // The positions of the markers
  double position(size_t index) const throw(std::out_of_range);

  // Accessors to markers
  const Marker &marker(size_t index) const throw(uninitialized_marker,
						 std::out_of_range);

  // Insert a marker at the position index -- this method only borrows
  // the reference, so don't free or change the marker after setting
  // it and before deleting the configuration -- and remember to free
  // it yourself after use of the configuration.
  void set_marker(size_t pos_index, const Marker *marker)
    throw(std::out_of_range);

  // Parameters for building the ARG and assigning mutations
  double rho()    const { return _rho; }
  double Q()      const { return _Q; }
  double G()      const { return _G; }
  double growth() const { return _growth; }

  double mu()     const { return _mu; }

  // Parameters for output
  bool print_all_nodes() const { return _print_all_nodes; }

  // For monitoring progress
  SimulationMonitor *monitor() const { return _monitor; }

private:
  // Disable these
  Configuration(const Configuration&);
  Configuration& operator = (const Configuration&);

  unsigned int _no_leaves;

  std::vector<double>  _positions;
  const Marker** _markers;

  double _rho;
  double _Q;
  double _G;
  double _growth;

  double _mu;

  bool _print_all_nodes;

  SimulationMonitor *_monitor; // 0 if we are not monitoring
};


template <typename InItr>
Configuration::Configuration(unsigned int no_leaves,
			     InItr begin, InItr end,
			     double rho, double Q, double G, double growth,
			     double mu,
			     bool print_all_nodes,
			     SimulationMonitor *monitor)
  throw(out_of_sequence)
  : _no_leaves(no_leaves),
    _positions(begin,end),
    _rho(rho), _Q(Q), _G(G), _growth(growth),
    _mu(mu),
    _print_all_nodes(print_all_nodes),
    _monitor(monitor)
{
  for (size_t m = 1; m < _positions.size(); ++m)
    if (_positions[m-1] >= _positions[m]) throw out_of_sequence();
  _markers = new const Marker* [no_markers()];
  for (size_t m = 0; m < no_markers(); ++m)
    _markers[m] = 0;
}

inline Configuration::~Configuration() 
{ delete[] _markers; }


inline double Configuration::position(size_t index)
  const throw(std::out_of_range)
{
  return _positions.at(index);
}

inline const Marker &Configuration::marker(size_t index)
  const throw(uninitialized_marker,std::out_of_range)
{
  if (index >= no_markers()) throw std::out_of_range("No marker at index");
  if (!_markers[index]) throw uninitialized_marker();
  else return *_markers[index];
}

inline void Configuration::set_marker(size_t index, const Marker *marker)
  throw(std::out_of_range)
{
  if (index >= no_markers()) throw std::out_of_range("No marker at index");
  _markers[index] = marker;
}


#endif // CONFIGURATION_HH_INCLUDED
