
#ifndef MARKER_HH_INCLUDED
#define MARKER_HH_INCLUDED

#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif

// Abstract class for the different possible marker types
class Marker
{
public:

  // Exception thrown if we try to add a value to a value set that
  // doesn't fit the type of the value set
  struct illegal_value : public std::logic_error {
    illegal_value() : std::logic_error("illegal marker value.") {}
  };

  virtual ~Marker() {};

  size_t size()     const { return _values.size(); }

  int value(size_t index)     const throw(std::out_of_range);
  virtual void add_value(int value) throw(illegal_value) = 0;

protected:
  Marker() {};
  std::vector<int> _values;

private:
  // Disable these
  Marker(const Marker&);
  Marker &operator = (const Marker&);
};

inline int Marker::value(size_t index) const throw (std::out_of_range)
{ return _values.at(index); }




#endif
