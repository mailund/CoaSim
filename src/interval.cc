
#include "interval.hh"

#ifndef VALARRAY_INCLUDED
# include <valarray>
# define VALARRAY_INCLUDED
#endif
#ifndef ALGORITHM_INCLUDED
# include <algorithm>
# define ALGORITHM_INCLUDED
#endif


using std::min; using std::max;


void Interval::check_empty() const throw(empty_interval)
{
  if (length() <= 0.0) throw empty_interval();
}

void Interval::check_range() const throw(interval_out_of_range)
{
  if ((start() < 0 or 1 <= start()) or (end() <= 0 or 1 < end()))
    throw interval_out_of_range();
}

Interval::Interval(double start, double end, unsigned int leaf_contacts)
  throw(empty_interval,interval_out_of_range)
    : _start(start), _end(end), _leaf_contacts(leaf_contacts)
{
  check_empty();
  check_range();
}



void Intervals::add(const Interval &i) throw(out_of_sequence)
{
  if (_intervals.size() == 0 or _intervals.back().end() <= i.start())
    _intervals.push_back(i);
  else
    throw out_of_sequence();
}

void Intervals::add(double start, double end, int contacts)
  throw(out_of_sequence,
	Interval::empty_interval, Interval::interval_out_of_range)
{
  Interval tmp(start,end,contacts);
  add(tmp);
}


// find the first interval that starts no later than point
std::vector<Interval>::const_iterator
Intervals::interval_starting_before(double point) const
{
  Interval dummy_interval(point,1.0);
  std::vector<Interval>::const_iterator itr;
  itr = lower_bound(_intervals.begin(),_intervals.end(), dummy_interval);

  // itr now points to the first interval that starts at or after point
  if (itr != _intervals.begin()) return --itr;
  else return itr;
}

// find the first interval that starts no earlier than point
std::vector<Interval>::const_iterator
Intervals::interval_starting_after(double point) const
{
  // special case to be able to handle the endpoint
  if (point == 1.0) return _intervals.end();

  Interval dummy_interval(point,1.0);
  std::vector<Interval>::const_iterator itr;
  itr = upper_bound(_intervals.begin(),_intervals.end(), dummy_interval);

  // itr now points to the first interval that starts at or after point
  if (itr != _intervals.end() and itr->start() == point) return ++itr;
  else return itr;
}

// check the (point)-predicate around point
bool Intervals::check_predicate(double point,
				interval_predicate_t predicate) const
{
  using std::bind2nd;   using std::mem_fun_ref;

  if (point == 1.0) // special case, needed to check for endpoint in 1.0
    return (_intervals.back().*predicate)(point);

  if (point < 0.0 or 1.0 <= point)
    throw std::out_of_range("checking point out of the [0,1) range.");

  std::vector<Interval>::const_iterator start, stop, res;
  start = interval_starting_before(point);
  stop = interval_starting_after(point);
  res = find_if(start,stop, bind2nd(mem_fun_ref(predicate),point));
  return res != stop;
}



// Adds the intervals where first comes before second
Intervals Intervals::add_ordered_intervals(Intervals const &first,
					   Intervals const &second)
  throw(out_of_sequence)
{
  if (first._intervals.back().overlaps(second._intervals.front()))
    throw out_of_sequence();

  Intervals result(first);
  std::copy(second._intervals.begin(), second._intervals.end(),
	    std::back_inserter(result._intervals));

  return result;
}


// this operator adds two intervals where all Interval on the one
// Intervals comes before all Interval on the second Intervals
Intervals Intervals::add_intervals(const Intervals &i) const
  throw(out_of_sequence)
{
  if (size() == 0)   return i;
  if (i.size() == 0) return *this;

  // At this point we know that both intervals are non-empty.  The
  // invariant of intervals gives us that they are ordered and
  // non-overlapping, if we concatenate them in the right order, the
  // invariant is still true.  If one does not not come before the
  // other, we must throw an exception
  
  Intervals result;
  
  if (_intervals.back().start() <= i._intervals.front().start())
    return add_ordered_intervals(*this,i);
  else if (i._intervals.back().start() <= _intervals.front().start())
    return add_ordered_intervals(i,*this);
  else
    throw out_of_sequence();

  return result;
}



// Copy the intervals between start and stop, trunkating the
// end-intervals to start and stop.
Intervals Intervals::copy(double start, double stop) const
  throw(illegal_interval)
{
  std::vector<Interval>::const_iterator first, last, itr;
  Intervals result;

  if (stop <= start) throw illegal_interval();
  if (start < 0.0)   throw illegal_interval();
  if (1.0 < stop)    throw illegal_interval();

  first = interval_starting_before(start);
  last = interval_starting_after(stop);

  // first points to the right-most interval that starts *before* start
  // last points to the left-most interval that starts *after* stop

  /* -- handle first -------------------------------------*/
  // if first contains start, we cut [start,first->end) -- or
  // [start,stop) if stop < first->end -- otherwise we just skip the
  // first interval; the next must be completely included as it must
  // start *after* start, according to the specification of
  // interval_starting_before: if it did not it would be to the right
  // of the right-most interval that starts before start

  if (start < first->end())
    if (stop < first->end())
      result.add(start, stop, first->leaf_contacts());
    else
      result.add(start, first->end(), first->leaf_contacts());
			      
  /* -- handle the rest ----------------------------------*/
  // the only special case is the last interval, where we must make
  // sure to cut at point stop
  for (itr = first + 1; itr != last; ++itr)
    {
      if (itr->end() <= stop)
	result.add(*itr);
      else
	{
	  // cut [itr->start(),stop) and add it, then terminate
	  if (stop < itr->end() and itr->start() < stop)
	    result.add(itr->start(),stop,itr->leaf_contacts());
	  break;
	}
    }

#if 0
  std::cout << "copy of [" << start << ',' << stop << "):\n";
  for (itr = result._intervals.begin(); itr != result._intervals.end(); ++itr)
    std::cout << '[' << itr->start() << ',' << itr->end() << ")\n";
#endif

  return result;
}

Intervals Intervals::merge(const Intervals& i) const
{
  std::vector<Interval> tmp_merge;

  std::merge(_intervals.begin(), _intervals.end(),
	     i._intervals.begin(), i._intervals.end(),
	     std::back_inserter(tmp_merge));

  if (tmp_merge.size() == 0) return Intervals(); // the empty merge

  std::vector<Interval> res_intervals;
  std::vector<Interval>::const_iterator itr = tmp_merge.begin();

  res_intervals.push_back(*itr);
  for (++itr; itr != tmp_merge.end(); ++itr)
    {
      if (! res_intervals.back().overlaps(*itr))
	res_intervals.push_back(*itr); // just move on to the next
      else
	{
	  // the last interval shouldn't really habe be added -- it
	  // overlaps the next -- so handle that
	  Interval interval = res_intervals.back(); res_intervals.pop_back();

	  double start1, end1;
	  double start2, end2;
	  double start3, end3;

	  start1 = interval.start();
	  start2 = end1 = itr->start();
	  start3 = end2 = interval.end();
	  end3 = itr->end();

	  // We know that interval 2 cannot be empty, as the two
	  // original intervals overlap, but both interval 1 and
	  // interval 3 can potentially be

	  try {
	    // The first interval covers the same leaves as `interval'
	    res_intervals.push_back(Interval(start1,end1,
					     interval.leaf_contacts()));
	  } catch (Interval::empty_interval&) {};
	  // The second interval joins the two intervals' trees
	  res_intervals.push_back(Interval(start2,end2,
					   interval.leaf_contacts()
					   +itr->leaf_contacts()));
	  try {
	    // The third interval covers the same leaves as `*itr'
	    res_intervals.push_back(Interval(start3,end3,
					     itr->leaf_contacts()));
	  } catch (Interval::empty_interval&) {};
	}
    }

#if 0
  std::cout << "Merge:\n";
  for (itr = res_intervals.begin(); itr != res_intervals.end(); ++itr)
    std::cout << '[' << itr->start() << ',' << itr->end() << ")\n";
#endif


  Intervals res; res._intervals = res_intervals;
  return res;
}

  

