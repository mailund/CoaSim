#include "interval.hh"
#include <valarray>
#include <iostream>

#define epsilon 1.0e-10

Intervals* Intervals::add_interval(Intervals* i_val)
{
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
};

Intervals* Intervals::operator+(Intervals in)
  // this operator adds two intervals where all Interval on the one Intervals comes before all Interval on the second Intervals 
{ 
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
};


bool Intervals::contains_point(double pos)
{
  std::vector< Interval* >::const_iterator i;
  for (i = _intervals.begin(); i != _intervals.end(); ++i)
    if (((*i)->start()<=pos) &&((*i)->end()>pos))
      return true;
  return false;
}  

Intervals* Intervals::copy(double start, double stop)
{
  Intervals* r = new Intervals();
  Interval* pr = 0;
  for (int i=0; i<size(); i++){
    pr = 0;

    // FIXME: this is only safe is the intervals are sorted -- is that
    // an invariant of the class?  otherwise, use continue instead of
    // break.
    if (_intervals[i]->start() >= stop) break;
    
    if ((_intervals[i]->start() < start) and (stop <= _intervals[i]->end())){
      pr = new Interval(start, stop-start,_intervals[i]->leaf_contacts());
    }
    else if((_intervals[i]->start() < start) and (start <= _intervals[i]->end())){
      pr = new Interval(start, _intervals[i]->end()-start,
			_intervals[i]->leaf_contacts());
    }
    else if((start <= _intervals[i]->start()) and (stop <= _intervals[i]->end())){
      pr = new Interval(_intervals[i]->start(),stop-_intervals[i]->start(),
			_intervals[i]->leaf_contacts());
    }
    else if((start <= _intervals[i]->start()) and (_intervals[i]->end() < stop)){
      pr = new Interval(_intervals[i]->start(),_intervals[i]->length(),
			_intervals[i]->leaf_contacts());
    }
    if (pr) r->add(pr);
  }
  return r;  
};

void Intervals::remove(int i)
{
  std::vector< Interval* > new_intervals(0);
  if ((i>=0)&&(i<size())){
    for (int j = 0; j<i; j++) new_intervals.push_back(_intervals[j]);
    if (i+1<size())
      for (int j = i+1; j<size(); j++) new_intervals.push_back(_intervals[j]);
  }
  _intervals.resize(0);
  _intervals.insert(_intervals.end(),new_intervals.begin(),new_intervals.end());
  //  for (std::vector< Interval* >::iterator i = new_intervals.begin(); i != new_intervals.end(); ++i) _intervals.push_back(*i);
  //  for (int j = 0; j<new_intervals.size(); j++) _intervals.push_back(new_intervals[j]);
}


class interval_start_comp
{
public:
  bool operator()(Interval* in1, Interval* in2){
    return (in1->start()<in2->start());
  }
};
  
std::vector<Interval*> Intervals::intervals_in_range(std::vector<Interval*> i_starts, double start, double stop)
{
  std::vector<Interval*> res(0);
  unsigned int index = 0;
  while (i_starts[index]->start()<stop+epsilon){
    if ((i_starts[index]->start()<start+epsilon)&&(i_starts[index]->end()+epsilon>stop)) res.push_back(i_starts[index]);
    index++;
    if (index>i_starts.size()-1) break;
  }
  return res;
}

Intervals* Intervals::merge(Intervals& in)
{
  Intervals* r = new Intervals();
  std::vector<Interval*> all_intervals_start(0);
  std::vector< double > all_intervals(0);
  std::vector< double > starts_and_stops(0);
  double start = 0.0;
  double stop = 0.0;
  std::vector<Interval* > between(0);
  int leaf_cnts = 0;

  // we are using that each marker up though the ancestral recombination graph may be seen as a binary tree.
  // this means that if two different intervals overlab, they connect to different leaf nodes, and the new combined
  // interval according connects to the sum of these leafnodes.

  if ((size()==0)&&(in.size()!=0)){
    for (int i=0; i<in.size(); i++)
      r -> add(new Interval(in[i].start(), in[i].length(), in[i].leaf_contacts()));
  }
  else if ((size()!=0)&&(in.size()==0)){
    for (int i=0; i<size(); i++)
      r -> add(new Interval(_intervals[i]->start(), _intervals[i]->length(), _intervals[i]->leaf_contacts()));
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
    
    for (unsigned int i=0; i<all_intervals.size()-1;i++)
      if (std::abs(all_intervals[i]-all_intervals[i+1])>epsilon/2){
	starts_and_stops.push_back(all_intervals[i+1]);
      } 
    

    for (int i=0;i<size();i++) all_intervals_start.push_back(_intervals[i]);
    for (int i=0;i<in.size();i++) all_intervals_start.push_back(&(in[i]));

    sort(all_intervals_start.begin(),all_intervals_start.end(),interval_start_comp());

    for (unsigned int i=0; i<starts_and_stops.size()-1; i++){      
      start = starts_and_stops[i];
      stop = starts_and_stops[i+1];
      between.resize(0);
      leaf_cnts = 0;
      between = intervals_in_range(all_intervals_start,start, stop);
      for (unsigned int j=0; j<between.size();j++)
	leaf_cnts = leaf_cnts + between[j]->leaf_contacts();
      if (leaf_cnts>0) r -> add(new Interval(start, stop-start, leaf_cnts));
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
      if (std::abs(all_intervals[i]-all_intervals[i+1])>epsilon){
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
  
bool Intervals::is_start(double point)
{
  bool res = false;
  for (int i=0; i<size(); i++){
    if (std::abs(_intervals[i]->start()-point)<epsilon){
      res = true;
      break;
    }
  }
  return res;
};

bool Intervals::is_end(double point)
{
  bool res = false;
  for (int i=0; i<size(); i++){
    if (std::abs(_intervals[i]->end()-point)<epsilon){
      res = true;
      break;
    }
  }
  return res;
};
