#include "interval.hh"
#include <valarray>
#include <iostream>

#include <algorithm>
using std::min; using std::max;


void Interval::check_empty() const throw(empty_interval)
{
  if (length() <= 0.0) throw empty_interval("Creating empty interval.");
}

void Interval::check_range() const throw(interval_out_of_range)
{
  if ((start() < 0 or 1 <= start()) or (end() <= 0 or 1 < end()))
    throw interval_out_of_range("Creating interval with illegal range.");
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
    throw out_of_sequence("Interval added out of sequence");
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

  if (point < 0.0 or 1.0 <= point)
    throw std::out_of_range("Point must be in [0,1).");

  std::vector<Interval>::const_iterator start, stop, res;
  start = interval_starting_before(point);
  stop = interval_starting_after(point);
  res = find_if(start,stop, bind2nd(mem_fun_ref(predicate),point));
  return res != stop;
}



// Adds the intervals where first comes before second, but where the
// last element of first might overlap the first of second (if that is
// the case the two intervals are joined).
Intervals Intervals::add_ordered_intervals(Intervals const &first,
					   Intervals const &second)
  throw(out_of_sequence)
{
  if (first._intervals.back().overlaps(second._intervals.front()))
    throw out_of_sequence("Addition of un-ordered intervals.");

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
    throw out_of_sequence("Adding intervals not ordered correctly.");

  return result;
}



// copy the intervals between start and stop, trunkating the
// end-intervals to start and stop.
Intervals Intervals::copy(double start, double stop) const
  throw(illegal_interval)
{
  std::vector<Interval>::const_iterator first, last, itr;
  Intervals result;

  if (stop <= start) throw illegal_interval("Copying in illegal interval.");
  if (start < 0.0)   throw illegal_interval("Copying in illegal interval.");
  if (1.0 <= stop)   throw illegal_interval("Copying in illegal interval.");

  first = interval_starting_before(start);
  last = interval_starting_after(stop);

  // first points to the right-most interval that starts *before* start
  // last points to the left-most interval that starts *after* stop

#if 0
  std::cout << "first: [" << first->start() << ',' << first->end() << ")\n"
	    << "last:  [" << last->start() << ',' << last->end() << ")\n";
#endif

  /* -- handle first -------------------------------------*/
  // if first contains start, we cut [start,first->end), otherwise we
  // just skip the first interval; the next must be completely
  // included as it must start *after* start, according to the
  // specification of interval_starting_before: if it did not it would
  // be to the right of the right-most interval that starts before
  // start

  if (start < first->end()) result.add(start,first->end(),
				       first->leaf_contacts());
			      
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

  // `interval' points to the interval we are currently processing,
  // `itr' to the next we will consider.  If `interval' does not
  // overlap `*itr', we insert `interval' and move on.  If `interval'
  // overlaps `*itr' we split the two intervals in three, insert the first
  // two and move on with the third.
  Interval interval = *itr;
  for (++itr; itr != tmp_merge.end(); ++itr)
    {
      if (interval.overlaps(*itr))
	{
	  double start1, end1;
	  double start2, end2;
	  double start3, end3;

	  start1 = interval.start();
	  start2 = end1 = itr->start();
	  start3 = end2 = interval.end();
	  end3 = itr->end();

	  // The first interval covers the same leaves as `interval'
	  // The second interval joins the two intervals' trees
	  // The third interval covers the same leaves as `*itr'

	  Interval i1(start1,end1,interval.leaf_contacts());
	  Interval i2(start2,end2,
		      interval.leaf_contacts()+itr->leaf_contacts());
	  Interval i3(start3,end3,itr->leaf_contacts());

	  res_intervals.push_back(i1);
	  res_intervals.push_back(i2);
	  interval = i3;
	}
      else
	{
	  res_intervals.push_back(interval);
	  interval = *itr;
	}
    }
  // remember to include the last interval
  res_intervals.push_back(interval);

  Intervals res; res._intervals = res_intervals;
  return res;
}

  


// FIXME: I haven't refactored this method yet!
std::vector<Interval> Intervals::intervals_in_range(std::vector<Interval> i_starts, double start, double stop)
{
  std::vector<Interval> res;
#if 0				// FIXME
  unsigned int index = 0;
  while (i_starts[index].start()<stop+Interval::epsilon){
    if ((i_starts[index].start()<start+Interval::epsilon)&&(i_starts[index].end()+Interval::epsilon>stop)) res.push_back(i_starts[index]);
    index++;
    if (index>i_starts.size()-1) break;
  }
#endif
  return res;
}

