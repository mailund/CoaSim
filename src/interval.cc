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

bool Intervals::is_start(double point) const
{
  return check_predicate(point, &Interval::is_start);
}

bool Intervals::is_end(double point) const
{
  return check_predicate(point, &Interval::is_end);
}

bool Intervals::contains_point(double point) const
{
  return check_predicate(point, &Interval::contains_point);
}



Intervals* Intervals::add_interval(Intervals* i_val)
{
  return 0;			// FIXME: add late
#if 0
  Intervals* r = new Intervals();
  
  if ((size()==0)&&(i_val -> size()!=0)){
    for (int i = 0; i<i_val -> size(); i++){
      r -> add(new Interval((i_val -> interval(i)).start(), (i_val -> interval(i)).length(), (i_val -> interval(i)).leaf_contacts()));
    }
  }
  if ((i_val -> size()==0)&&(size()!=0)){
    for (int i = 0; i<size(); i++){
      r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
    }
  }
  if ((i_val -> size()!=0)&&(size()!=0)){    
    if (_intervals[size()-1]->end()<(i_val -> interval(0)).start()){
      for (int i = 0; i<size(); i++){
	r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
      }
      for (int i = 0; i<i_val -> size(); i++){
	r -> add(new Interval((i_val -> interval(i)).start(), (i_val -> interval(i)).length(), (i_val -> interval(i)).leaf_contacts()));
      }
    }
    else {
      for (int i = 0; i<i_val -> size(); i++){
	r -> add(new Interval((i_val -> interval(i)).start(), (i_val -> interval(i)).length(), (i_val -> interval(i)).leaf_contacts()));
      }
      for (int i = 0; i<size(); i++){
	r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
      }
    }
  }
  return r;
#endif
};

Intervals* Intervals::operator+(Intervals &in)
  // this operator adds two intervals where all Interval on the one Intervals comes before all Interval on the second Intervals 
{
  return 0;
#if 0
  Intervals* r = new Intervals();
  
  if ((size()==0)&&(in.size()!=0)){
    for (int i = 0; i<in.size(); i++){
      r -> add(new Interval(in[i].start(), in[i].length(), in[i].leaf_contacts()));
    }
  }
  if ((in.size()==0)&&(size()!=0)){
    for (int i = 0; i<size(); i++){
      r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
    }
  }
  if ((in.size()!=0)&&(size()!=0)){    
    if (_intervals[size()-1]->end()<in[0].start()){
      for (int i = 0; i<size(); i++){
	r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
      }
      for (int i = 0; i<in.size(); i++){
	r -> add(new Interval(in[i].start(), in[i].length(), in[i].leaf_contacts()));
      }
    }
    else {
      for (int i = 0; i<in.size(); i++){
	r -> add(new Interval(in[i].start(), in[i].length(), in[i].leaf_contacts()));
      }
      for (int i = 0; i<size(); i++){
	r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
      }
    }
  }
  return r;
#endif
};



// copy the intervals between start and stop, trunkating the
// end-intervals to start and stop.
Intervals Intervals::copy(double start, double stop) throw(illegal_interval)
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



class interval_start_comp
{
public:
  bool operator()(const Interval &in1, const Interval &in2){
    return in1.start() < in2.start();
  }
};
  
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

Intervals* Intervals::merge(Intervals& in)
{
  Intervals* r = new Intervals();
  std::vector< Interval > all_intervals_start;
  std::vector< double > all_intervals;
  std::vector< double > starts_and_stops;
  double start = 0.0;
  double stop = 0.0;
  std::vector< Interval > between;
  int leaf_cnts = 0;

  // we are using that each marker up though the ancestral recombination graph may be seen as a binary tree.
  // this means that if two different intervals overlab, they connect to different leaf nodes, and the new combined
  // interval according connects to the sum of these leafnodes.

  if ((size()==0)&&(in.size()!=0)){
    for (int i=0; i<in.size(); i++)
      r -> add(in[i].start(), in[i].length(), in[i].leaf_contacts());
  }
  else if ((size()!=0)&&(in.size()==0)){
    for (int i=0; i<size(); i++)
      r -> add(_intervals[i].start(), _intervals[i].length(), _intervals[i].leaf_contacts());
  }
  else if ((size()!=0)&&(in.size()!=0)){
    for (int i=0;i<size();i++){
      all_intervals.push_back(_intervals[i].start());
      all_intervals.push_back(_intervals[i].end());
    }
  
    for (int i=0;i<in.size();i++){
      all_intervals.push_back(in[i].start());
      all_intervals.push_back(in[i].end());
    }
    sort(all_intervals.begin(),all_intervals.end());
    
    starts_and_stops.push_back(all_intervals[0]);
    
    for (unsigned int i=0; i<all_intervals.size()-1;i++)
      if (std::abs(all_intervals[i]-all_intervals[i+1])>Interval::epsilon/2){
	starts_and_stops.push_back(all_intervals[i+1]);
      } 
    

    for (int i=0;i<size();i++) all_intervals_start.push_back(_intervals[i]);
    for (int i=0;i<in.size();i++) all_intervals_start.push_back(in[i]);

    sort(all_intervals_start.begin(),all_intervals_start.end(),interval_start_comp());

    for (unsigned int i=0; i<starts_and_stops.size()-1; i++){      
      start = starts_and_stops[i];
      stop = starts_and_stops[i+1];
      between.clear();
      leaf_cnts = 0;
      between = intervals_in_range(all_intervals_start,start, stop);
      for (unsigned int j=0; j<between.size();j++)
	leaf_cnts = leaf_cnts + between[j].leaf_contacts();
      if (leaf_cnts>0) r -> add(start, stop-start, leaf_cnts);
    }
  }
  return r;
}

/*
Intervals Intervals::merge(Intervals& in)
{
  Intervals r;
  int first_index = 0;
  int second_index = 0;
  std::vector< double > all_intervals(0);
  std::vector< double > starts_and_stops(0);
  double middle = 0.0;

  if ((size()==0)&&(in.size()!=0)){
    for (int i=0; i<in.size(); i++)
      r.add(new Interval(in[i].start(), in[i].length(), in[i].leaf_contacts()));
  }
  else if ((size()!=0)&&(in.size()==0)){
    for (int i=0; i<size(); i++)
      r.add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
  }
  else if ((size()!=0)&&(in.size()!=0)){
    for (int i=0;i<size();i++){
      all_intervals.push_back(_intervals[i]->start());
      all_intervals.push_back(_intervals[i]->end());
    }
  
    for (int i=0;i<in.size();i++){
      all_intervals.push_back(in[i].start());
      all_intervals.push_back(in[i].end());
    }
    sort(all_intervals.begin(),all_intervals.end());
    
    starts_and_stops.push_back(all_intervals[0]);
    
    for (int i=0; i<all_intervals.size()-1;i++)
      if (std::abs(all_intervals[i]-all_intervals[i+1])>Interval::epsilon){
	starts_and_stops.push_back(all_intervals[i+1]);
      } 
	
    std::valarray< int > is_first_on(starts_and_stops.size());
    std::valarray< int > is_second_on(starts_and_stops.size());
    for (int i=0; i<starts_and_stops.size()-1; i++){      
      middle = (starts_and_stops[i]+starts_and_stops[i+1])/2.0;
      if ((_intervals[first_index]->start()<=middle)&&(_intervals[first_index]->end()>middle))
	is_first_on[i] = _intervals[first_index]->leaf_contacts();      
      else is_first_on[i] = 0;
      if ((in[second_index].start()<=middle)&&(in[second_index].end()>middle))
	is_second_on[i] = in[second_index].leaf_contacts();      
      else is_second_on[i] = 0;
      if ((first_index<size()-1)&&(_intervals[first_index]->end()<middle)) first_index++;
      if ((second_index<in.size()-1)&&(in[second_index].end()<middle)) second_index++;
    }
    
    
    for (int i=0; i<starts_and_stops.size()-1; i++){      
      if (is_first_on[i]+is_second_on[i]>0) {
	r.add(new Interval(starts_and_stops[i],starts_and_stops[i+1]-starts_and_stops[i],is_first_on[i]+is_second_on[i]));
      }
    }
  }
  return r;
};
  
*/
  
