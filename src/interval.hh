#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <stdexcept>
#include <vector>
#include <iostream>

/* A sub-interval on the real interval [0,1), closed on the left and
 * open on the right (not that it matters much, as we have an epsilon
 * uncertainty around the end-points...). */
class Interval
{
public:

  // used for checking double "equality"
  static const double epsilon = 1.0e-10;

  // exception thrown if the interval is empty (or negative) -- since
  // it must be closed in one end and open in the other, it cannot be
  // a point.
  class empty_interval : public std::exception {};
  // exception thrown in the interval is not in the range [0,1)
  class interval_out_of_range : public std::exception {};

  // exception thrown if we try to merge two non-overlapping intervals
  class non_overlapping : public std::exception {};


  Interval(double start, double length, unsigned int leaf_contacts = 0)
    throw(empty_interval,interval_out_of_range);
  ~Interval() {}

  double start()  const { return _start; }
  double length() const { return _length; }
  double end()    const { return _start + _length; }

  int leaf_contacts() const { return _leaf_contacts; }


  // it is better to use these for comparison as exact double equality
  // is hard to get.
  bool is_start(double point) const;
  bool is_end(double point)   const;


  // returns whether point is in the interval (or no longer than
  // epsilon from it)
  bool contains_point(double point) const;
  // returns whether i overlaps this interval (or is no longer than
  // epsilon from it)
  bool overlaps(const Interval &i) const;


  // join this and i, assuming they overlap (using the overlaps()
  // test), i.e. overlap or at least epsilon close
  void join(const Interval &i) throw(non_overlapping);
  Interval & operator |= (const Interval &i) throw(non_overlapping);

  bool operator == (const Interval &i) const;
  bool operator != (const Interval &i) const;

  // i1 < i2 if i1.start() < i2.start(), independent of other values.
  bool operator < (const Interval &i)  const;

private:
  void check_empty() const throw(empty_interval);
  void check_range() const throw(interval_out_of_range);

  double _start;
  double _length;
  unsigned int _leaf_contacts; // number of leaf nodes that this
			       // interval connects to
};

inline bool Interval::is_start(double point) const
{ return std::abs(start()-point) < epsilon; }
inline bool Interval::is_end(double point) const
{ return std::abs(end()-point) < epsilon; }


inline bool Interval::contains_point(double point) const
{ return (start()-epsilon <= point) and (point < end()+epsilon); }

inline bool Interval::overlaps(const Interval &i) const
{ return (start() <= i.start()) ? (i.start() < (end()+epsilon))
                                : (start() < (i.end()+epsilon)); }

inline Interval & Interval::operator |= (const Interval &i)
  throw(non_overlapping)
{ join(i); return *this; }

inline Interval operator | (const Interval &i1, const Interval &i2)
  throw(Interval::non_overlapping)
{ Interval res(i1); return res |= i2; }

inline bool Interval::operator == (const Interval &i) const
{ return ((std::abs(_start-i._start) < epsilon)
	  and (std::abs(_length - i._length) < epsilon)
	  and (_leaf_contacts == i._leaf_contacts)); }

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
  class out_of_sequence : public std::exception {};

  // exception thrown if we try to copy an empty or inverted
  // (stop<=start) sub-interval.
  class illegal_interval : public std::exception {};


  // add an interval to the Intervals -- the added interval must start
  // later than the previously added intervals; if it overlaps the
  // last interval so far, it will be joined with it.
  void add(const Interval &i) throw(out_of_sequence);
  void add(double start, double length, int contacts) throw(out_of_sequence);

  // these methods looks up the intervals NOT CHECKING if the index is
  // within range!
  const Interval& interval(int index)     const { return _intervals[index]; }
  const Interval& operator [] (int index) const { return interval(index); }

  int size() const { return _intervals.size(); }

  bool is_start(double point)     const;
  bool is_end(double point)       const;
  bool contains_point(double pos) const;

  void reset() { _intervals.clear(); }

  // copy the intervals between start and stop, trunkating intervals
  // that overlap start and stop.
  Intervals copy(double start, double stop) const throw(illegal_interval);

  // merge this and i, joining overlapping intervals
  Intervals merge(const Intervals& i) const;
  Intervals operator | (const Intervals &i) const;

  // This method adds two intervals where all Interval on the one
  // Intervals comes before all Interval on the second Intervals;
  // throws an exception if one interval does not come before the
  // other (but merges the end of one and the start of the next if
  // they overlap).
  Intervals add_intervals(const Intervals &i) const throw(out_of_sequence);
  Intervals operator + (const Intervals &i)   const throw(out_of_sequence);


  // FIXME: I am not sure how this method is used, but I think that it
  // might be better as a version of copy (?)
  static std::vector<Interval> intervals_in_range(std::vector<Interval> i_starts, double start, double stop);


private:

  // INVARIANT: The _intervals vector contains the non-overlapping
  // intervals in sorted order, wrt to < on intervals.
  std::vector<Interval> _intervals;

  std::vector<Interval>::const_iterator interval_starting_before(double point) const;
  std::vector<Interval>::const_iterator interval_starting_after(double point) const;
  typedef bool (Interval::*interval_predicate_t)(double point) const;
  bool check_predicate(double point, interval_predicate_t predicate) const;

  // Adds the intervals where first comes before second, but where the
  // last element of first might overlap the first of second (if that
  // is the case the two intervals are joined).
  static Intervals add_ordered_intervals(Intervals const &first,
					 Intervals const &second);
					 
};


inline bool Intervals::is_start(double point) const
{ return check_predicate(point, &Interval::is_start); }

inline bool Intervals::is_end(double point) const
{ return check_predicate(point, &Interval::is_end); }

inline bool Intervals::contains_point(double point) const
{ return check_predicate(point, &Interval::contains_point); }

inline Intervals Intervals::operator | (const Intervals &i) const
{ return merge(i); }

inline Intervals Intervals::operator + (const Intervals &in) const
  throw(out_of_sequence)
{ return add_intervals(in); }


#endif
