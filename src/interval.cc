#include "interval.hh"
#include <valarray>
#include <iostream>

#include <algorithm>
using std::min; using std::max;


void Interval::check_empty() const throw(empty_interval)
{
  if (_length <= 0.0) throw empty_interval();
}

void Interval::check_range() const throw(interval_out_of_range)
{
  if ((start() < 0 or 1 <= start()) or (end() <= 0 or 1 < end()))
    throw interval_out_of_range();
}

Interval::Interval(double start, double length, unsigned int leaf_contacts)
  throw(empty_interval,interval_out_of_range)
    : _start(start), _length(length), _leaf_contacts(leaf_contacts)
{
  check_empty();
  check_range();
}

void Interval::join(const Interval &i) throw(non_overlapping)
{
  if (!overlaps(i)) throw non_overlapping();

  double join_start = min(start(),i.start());
  double join_end = max(end(),i.end());
  // we are using that each marker up though the ancestral
  // recombination graph may be seen as a binary tree.  this means
  // that if two different intervals overlab, they connect to
  // different leaf nodes, and the new combined interval according
  // connects to the sum of these leafnodes.
  unsigned int join_leaf_contacts = leaf_contacts()+i.leaf_contacts();

  _start = join_start;
  _length = join_end-join_start;
  _leaf_contacts = join_leaf_contacts;
}





void Intervals::add(const Interval &i) throw(out_of_sequence)
{
  if (_intervals.size() == 0)
      _intervals.push_back(i);
  else
    {
      Interval &last = _intervals.back();
      if (i.start() < last.start()) throw out_of_sequence();
      if (last.overlaps(i)) last |= i;
      else _intervals.push_back(i);
    }
}

void Intervals::add(double start, double length, int contacts)
  throw(out_of_sequence)
{
  Interval tmp(start,length,contacts);
  add(tmp);
}


// find the first interval that starts no later than point-epsilon
std::vector<Interval>::const_iterator
Intervals::interval_starting_before(double point) const
{
  double start = max(min(1.0-Interval::epsilon,point-Interval::epsilon),0.0);

  Interval dummy_interval(start,Interval::epsilon);
  std::vector<Interval>::const_iterator itr;
  itr = lower_bound(_intervals.begin(),_intervals.end(), dummy_interval);
  if (itr == _intervals.begin()) return itr;
  else return --itr;
}

// find the first interval that starts no earlier than point+epsilon
std::vector<Interval>::const_iterator
Intervals::interval_starting_after(double point) const
{
  double start = max(min(1.0-Interval::epsilon,point+Interval::epsilon),0.0);
  Interval dummy_interval(start,Interval::epsilon);
  return lower_bound(_intervals.begin(),_intervals.end(), dummy_interval);
}

// check the (point)-predicate around point
bool Intervals::check_predicate(double point,
				interval_predicate_t predicate) const
{
  using std::bind2nd;   using std::mem_fun_ref;
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
{
  Intervals result(first);
  if (result._intervals.back().overlaps(second._intervals.front()))
    {
      result._intervals.back() |= second._intervals.front();
      std::copy(second._intervals.begin() + 1, second._intervals.end(),
		std::back_inserter(result._intervals));
    }
  else
    {
      std::copy(second._intervals.begin(), second._intervals.end(),
		std::back_inserter(result._intervals));
    }
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
    throw out_of_sequence();	// the intervals are not ordered correctly

  return result;
}



// copy the intervals between start and stop, trunkating the
// end-intervals to start and stop.
Intervals Intervals::copy(double start, double stop) const
  throw(illegal_interval)
{
  std::vector<Interval>::const_iterator first, last, itr;
  Intervals result;

  if (stop <= start) throw illegal_interval();
  if (start < 0.0)   throw illegal_interval();
  if (1.0 <= stop)   throw illegal_interval();

  first = interval_starting_before(start);
  last = interval_starting_after(stop);

  // handle up to start
  for (itr = first; itr != last; ++itr)
    if (start < itr->end())
      {
	// cut [start,itr->end()) and add it
	result.add(start,itr->end()-start,itr->leaf_contacts());
	break;
      }

  // handle the rest
  for (++itr; itr != last; ++itr)
    {
      if (itr->end() < stop)
	result.add(*itr);
      else
	{
	  // cut [itr->start(),stop) and add it, then terminate
	  if (stop < itr->end() and itr->start() < stop)
	    result.add(itr->start(),stop-itr->start(),itr->leaf_contacts());
	  break;
	}
    }

  return result;
}


Intervals Intervals::merge(const Intervals& i) const
{
  std::vector<Interval> tmp_merge;

  std::merge(_intervals.begin(), _intervals.end(),
	     i._intervals.begin(), i._intervals.end(),
	     std::back_inserter(tmp_merge));

  if (tmp_merge.size() == 0) return Intervals(); // the empty merge

  // otherwise, just join the overlapping intervals
  std::vector<Interval> res_intervals;
  std::vector<Interval>::const_iterator itr = tmp_merge.begin();

  res_intervals.push_back(*itr);
  for (++itr; itr != tmp_merge.end(); ++itr)
    if (res_intervals.back().overlaps(*itr))
      res_intervals.back() |= *itr;
    else
      res_intervals.push_back(*itr);

  Intervals res; res._intervals = res_intervals;
  return res;
}

  


// FIXME: I haven't refactored this method yet!
std::vector<Interval> Intervals::intervals_in_range(std::vector<Interval> i_starts, double start, double stop)
{
  std::vector<Interval> res;
  unsigned int index = 0;
  while (i_starts[index].start()<stop+Interval::epsilon){
    if ((i_starts[index].start()<start+Interval::epsilon)&&(i_starts[index].end()+Interval::epsilon>stop)) res.push_back(i_starts[index]);
    index++;
    if (index>i_starts.size()-1) break;
  }
  return res;
}

