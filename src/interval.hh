#ifndef INTERVAL_HH_INCLUDED
#define INTERVAL_HH_INCLUDED

#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif
#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif

/* A sub-interval on the real interval [0,1), closed on the left and
 * open on the right. */
class Interval
{
public:

  // exception thrown if the interval is empty (or negative) -- since
  // it must be closed in one end and open in the other, it cannot be
  // a point.
  struct empty_interval : public std::logic_error {
    empty_interval() : std::logic_error("empty interval.") {}
  };
  // exception thrown in the interval is not in the range [0,1)
  struct interval_out_of_range : public std::logic_error {
    interval_out_of_range() : std::logic_error("interval out of range.") {}
  };

  Interval(double start, double end, unsigned int leaf_contacts = 0)
    throw(empty_interval,interval_out_of_range);
  ~Interval() {}

  double start()  const { return _start; }
  double end()    const { return _end; }
  double length() const { return _end - _start; }

  unsigned int leaf_contacts() const { return _leaf_contacts; }


  // it is better to use these for comparison as exact double equality
  // is hard to get.
  bool is_start(double point) const { return point == _start; }
  bool is_end(double point)   const { return point == _end; }


  // returns whether point is in the interval
  bool contains_point(double point) const;
  // returns whether i overlaps this interval
  bool overlaps(const Interval &i) const;


  bool operator == (const Interval &i) const;
  bool operator != (const Interval &i) const;

  // i1 < i2 if i1.start() < i2.start(), independent of other values.
  bool operator < (const Interval &i)  const;

private:
  void check_empty() const throw(empty_interval);
  void check_range() const throw(interval_out_of_range);

  double _start;
  double _end;
  unsigned int _leaf_contacts; // number of leaf nodes that this
			       // interval connects to
};


inline bool Interval::contains_point(double point) const
{ return (start() <= point) and (point < end()); }

inline bool Interval::overlaps(const Interval &i) const
{ return (start() <= i.start()) ? (i.start() < end()) : (start() < i.end()); }


inline bool Interval::operator == (const Interval &i) const
{ return (_start == i._start) and (_end == i._end)
	  and (_leaf_contacts == i._leaf_contacts); }

inline bool Interval::operator != (const Interval &i) const
{ return !(*this == i); }
inline bool Interval::operator < (const Interval &i) const
{ return _start < i._start; }



/* A set of non-overlapping intervals. */
class Intervals
{
public:
  // exception thrown if an interval is added out of sequence (i.e. is
  // less than a previous added interval.
  struct out_of_sequence : public std::logic_error {
    out_of_sequence() : std::logic_error("intervals out of sequence.") {}
  };

  // exception thrown if we try to copy an empty or inverted
  // (stop<=start) sub-interval.
  struct illegal_interval : public std::logic_error {
    illegal_interval() : std::logic_error("illegal interval.") {}
  };

  // add an interval to the Intervals -- the added interval must start
  // later than the previously added intervals.
  void add(const Interval &i) throw(out_of_sequence);
  void add(double start, double end, int contacts = 0)
    throw(out_of_sequence, Interval::empty_interval,
	  Interval::interval_out_of_range);

  // these methods looks up the intervals NOT CHECKING if the index is
  // within range!
  const Interval& interval(int index)     const { return _intervals[index]; }
  const Interval& operator [] (int index) const { return interval(index); }

  int size() const { return _intervals.size(); }

  // checking the predicates on the relevant intervals.  Throws an
  // exception if point is outside [0,1).
  bool is_start(double point)     const throw(std::out_of_range);
  bool is_end(double point)       const throw(std::out_of_range);
  bool contains_point(double pos) const throw(std::out_of_range);

  // the first point in the first interval (the left most point)
  double first_point() const throw(std::out_of_range);
  // the last point in the last interval (the right most point)
  double last_point()  const throw(std::out_of_range);

  void reset() { _intervals.clear(); }

  // copy the intervals between start and stop, trunkating intervals
  // that overlap start and stop.
  Intervals copy(double start, double stop) const throw(illegal_interval);

  // merge this and i, splitting overlapping intervals
  Intervals merge(const Intervals& i) const;
  Intervals operator | (const Intervals &i) const;

  // This method adds two intervals where all Interval on the one
  // Intervals comes before all Interval on the second Intervals;
  // throws an exception if one interval does not come before the
  // other.
  Intervals add_intervals(const Intervals &i) const throw(out_of_sequence);
  Intervals operator + (const Intervals &i)   const throw(out_of_sequence);

private:

  // INVARIANT: The _intervals vector contains the non-overlapping
  // intervals in sorted order, wrt to < on intervals.
  std::vector<Interval> _intervals;

  std::vector<Interval>::const_iterator interval_starting_before(double point) const;
  std::vector<Interval>::const_iterator interval_starting_after(double point) const;
  typedef bool (Interval::*interval_predicate_t)(double point) const;
  bool check_predicate(double point, interval_predicate_t predicate) const;

  // Adds the intervals where first comes before second. If first
  // overlaps second, an exception is thrown
  static Intervals add_ordered_intervals(Intervals const &first,
					 Intervals const &second)
    throw(out_of_sequence);
					 
};


inline bool Intervals::is_start(double point) const throw(std::out_of_range)
{ return check_predicate(point, &Interval::is_start); }

inline bool Intervals::is_end(double point) const throw(std::out_of_range)
{ return check_predicate(point, &Interval::is_end); }

inline bool Intervals::contains_point(double point) const
  throw(std::out_of_range)
{ return check_predicate(point, &Interval::contains_point); }

inline double Intervals::first_point() const throw(std::out_of_range)
{
  if (_intervals.size() == 0) throw std::out_of_range("no intervals!");
  return _intervals.front().start();
}
inline double Intervals::last_point() const throw(std::out_of_range)
{
  if (_intervals.size() == 0) throw std::out_of_range("no intervals!");
  return _intervals.back().end();
}


inline Intervals Intervals::operator | (const Intervals &i) const
{ return merge(i); }

inline Intervals Intervals::operator + (const Intervals &in) const
  throw(out_of_sequence)
{ return add_intervals(in); }


#endif
