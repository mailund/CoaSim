#ifndef INTERVAL_HH
#define INTERVAL_HH
#include <vector>
#include <iostream>

class Interval
{
public:
  Interval(double s, double l, int cnt = 0): _start(s), _length(l), _leaf_contacts(cnt) {};
  ~Interval(){
    //    std::cout << "Interval destructor called" << std::endl;
  };
  double start(){ return _start; };
  void set_start(double s){ _start = s; };
  double length(){ return _length; };
  void set_length(double l){ _length = l;};
  int leaf_contacts(){ return _leaf_contacts; };
  void set_leaf_contacts(int lc){ _leaf_contacts = lc; };
  double end(){ return _start + _length; };
private:
  double _start;
  double _length;
  int _leaf_contacts; // number of leaf nodes that this interval connects to
};


class Intervals
{
public:
  Intervals(){};
  ~Intervals() { reset(); };

  Interval& operator[] (int index) {return *_intervals[index]; };
  Interval& interval(int index) { return *_intervals[index]; };

  void add(Interval* i){ _intervals.push_back(i);};

  // copy the intervals between start and stop, trunkating intervals
  // that overlap start and stop.
  Intervals* copy(double start, double stop);

  Intervals* operator+(Intervals in);

  Intervals* merge(Intervals& in);

  Intervals* add_interval(Intervals* i_val);

  void reset() {
    Interval* ival;
    for (unsigned int i=0; i<_intervals.size(); i++) {
      ival = _intervals[i];
      delete ival;		// FIXME: this copy construction makes
				// assignment and the copy constructor
				// dangerous!
    }
    _intervals.resize(0);
  };

  int size(){ return _intervals.size();};
  void remove(int i);
  bool is_start(double point);
  bool is_end(double point);
  bool contains_point(double pos);

  static std::vector<Interval*> intervals_in_range(std::vector<Interval*> i_starts, double start, double stop);

private:
  std::vector< Interval* > _intervals;
};


#endif
