
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
#ifndef CASSERT_INCLUDED
# include <cassert>
# define CASSERT_INCLUDED
#endif

class Configuration;
class RetiredInterval;
class Node;

// Abstract class for mutating the ARG
class Mutator
{
public:
    // Exception thrown if a mutation should be re-tried (for example,
    // because the mutation frequency is outside the desired range
    struct retry_mutation : public std::exception { retry_mutation() {} };

    // Exception thrown if the entire ARG should be rebuilt for another
    // try (for example, because the trait mutation frequency is outside
    // the desired range
    struct retry_arg : public std::exception { retry_arg() {} };
  

    virtual bool edge_has_mutation(double parent_time, double child_time) = 0;
    virtual int  mutate_to(const Node &n, unsigned int marker_index)      = 0;
};

// Abstract class for the different possible marker types
class Marker
{
public:

    // Exception thrown if we try to add a value to a value set that
    // doesn't fit the type of the value set
    struct illegal_value : public std::logic_error {
	illegal_value() : std::logic_error("illegal marker value.") {}
    };

    // polymorphic copying
    virtual Marker *copy() const = 0;

    virtual ~Marker();

    double position() const { return i_position; }
    virtual bool run_first() const = 0;
    
    size_t size()     const { return i_values.size(); }

    virtual int default_value() const = 0;
    int value(size_t index)     const throw(std::out_of_range);
    virtual void add_value(int value) throw(illegal_value) = 0;

    // creates a new mutator -- the mutator must be deleted after use.
    virtual Mutator *create_mutator(const Configuration &conf,
				    const RetiredInterval &ri) const = 0;

    virtual void to_text(std::ostream &os) const = 0;

protected:
    Marker(double position) : i_position(position) { assert(0 <= position); assert(position < 1); };
    double i_position;
    std::vector<int> i_values;

    Marker(const Marker&);

private:
    // Disable
    Marker &operator = (const Marker&);
};

inline int Marker::value(size_t index) const throw (std::out_of_range)
{ return i_values.at(index); }

inline std::ostream &operator << (std::ostream &os, const Marker &m)
{ m.to_text(os); return os; }


#endif
