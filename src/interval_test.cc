
#include "interval.hh"

#include <cstdlib>
#include <cassert>

static void test_Interval()
{
  double start = 0.0, length = 10.0;
  int contacts = 2;

  Interval i(start,length,contacts);

  assert(i.start() == start);
  assert(i.length() == length);
  assert(i.end() == start + length);

  start = 10.0; i.set_start(start);
  assert(i.start() == start);
  assert(i.length() == length);
  assert(i.end() == start + length);

  length = 20.3; i.set_length(length);
  assert(i.start() == start);
  assert(i.length() == length);
  assert(i.end() == start + length);

  assert(i.leaf_contacts() == contacts);
  contacts = 4; i.set_leaf_contacts(contacts);
  assert(i.leaf_contacts() == contacts);
}

static void test_Intervals()
{
  //    00   10   20   30   40   50
  // 0: |---------|
  // 1:      |----|
  // 2:           |----|
  // 3:                |---------|
  // 4                      |----|
  static double starts[] =  {  0, 10, 20, 30, 40, };
  static double lengths[] = { 20, 10, 10, 20, 10, };

  // be careful here!  after these have been added to the Intervals,
  // the Intervals owns the pointers and it will delete them when it
  // is reset or destroyed.
  Interval* interval_array[] = {
    new Interval(starts[0],lengths[0]),
    new Interval(starts[1],lengths[1]),
    new Interval(starts[2],lengths[2]),
    new Interval(starts[3],lengths[3]),
    new Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  assert(intervals.size() == no_intervals);

  for (int i = 0; i < no_intervals; ++i)
    {
      assert(&intervals[i] == interval_array[i]);
      assert(&intervals.interval(i) == interval_array[i]);
    }

  for (int i = 0; i < no_intervals; ++i)
    assert(intervals.is_start(starts[i]));
  for (int i = 0; i < no_intervals; ++i)
    assert(intervals.is_end(starts[i]+lengths[i]));

  for (int i = 0; i < no_intervals; ++i)
    assert(intervals.contains_point(starts[i]+lengths[i]/2));
  assert(! intervals.contains_point(starts[0] - 2.0));
  // this test only works as long as the last interval's endpoint is
  // the last point
  assert(!intervals.contains_point(starts[no_intervals]+lengths[no_intervals]+2.0));


  intervals.remove(2);
  assert(intervals.size() == no_intervals - 1);
  // this test only works as long as other intervals doesn't overlap
  // interval 2
  assert(! intervals.contains_point(starts[2] + lengths[2]/2));

  // FIXME: copy
  // FIXME: + 
  // FIXME: merge
  // FIXME: add_interval


  intervals.reset();
  assert(intervals.size() == 0);
}

static void test_intervals_in_range()
{
  //    00   10   20   30   40   50
  // 0: |---------|
  // 1:      |----|
  // 2:           |----|
  // 3:                |---------|
  // 4                      |----|
  static double starts[] =  {  0, 10, 20, 30, 40, };
  static double lengths[] = { 20, 10, 10, 20, 10, };

  // be careful here!  after these have been added to the Intervals,
  // the Intervals owns the pointers and it will delete them when it
  // is reset or destroyed.
  Interval* interval_array[] = {
    new Interval(starts[0],lengths[0]),
    new Interval(starts[1],lengths[1]),
    new Interval(starts[2],lengths[2]),
    new Interval(starts[3],lengths[3]),
    new Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  // FIXME: check GC here -- are the intervals copied or are new
  // intervals allocated?
  std::vector< Interval* > intervals(interval_array, interval_array+no_intervals);
  std::vector< Interval* > is1, is2, is3, is4, is5;

  // -- testing middle of intervals --------------------------------

  is1 = Intervals::intervals_in_range(intervals, 11, 19);

  //FIXME: assert(is1.size() == 2);
  assert(find(is1.begin(),is1.end(),interval_array[0]) != is1.end());
  //FIXME: assert(find(is1.begin(),is1.end(),interval_array[1]) != is1.end());
  assert(find(is1.begin(),is1.end(),interval_array[2]) == is1.end());
  assert(find(is1.begin(),is1.end(),interval_array[3]) == is1.end());
  assert(find(is1.begin(),is1.end(),interval_array[4]) == is1.end());

  is2 = Intervals::intervals_in_range(intervals, 21, 24);

  //FIXME: assert(is2.size() == 1);
  assert(find(is2.begin(),is2.end(),interval_array[0]) == is2.end());
  assert(find(is2.begin(),is2.end(),interval_array[1]) == is2.end());
  //FIXME: assert(find(is2.begin(),is2.end(),interval_array[2]) != is2.end());
  assert(find(is2.begin(),is2.end(),interval_array[3]) == is2.end());
  assert(find(is2.begin(),is2.end(),interval_array[4]) == is2.end());

  // -- testing end of intervals -----------------------------------
  is3 = Intervals::intervals_in_range(intervals, 0, 0.1);

  assert(is3.size() == 1);
  assert(find(is3.begin(),is3.end(),interval_array[0]) != is3.end());
  assert(find(is3.begin(),is3.end(),interval_array[1]) == is3.end());
  assert(find(is3.begin(),is3.end(),interval_array[2]) == is3.end());
  assert(find(is3.begin(),is3.end(),interval_array[3]) == is3.end());
  assert(find(is3.begin(),is3.end(),interval_array[4]) == is3.end());

  is4 = Intervals::intervals_in_range(intervals, 50, 50.1);
  
  //FIXME: assert(is4.size() == 2);
  assert(find(is4.begin(),is4.end(),interval_array[0]) == is4.end());
  assert(find(is4.begin(),is4.end(),interval_array[1]) == is4.end());
  assert(find(is4.begin(),is4.end(),interval_array[2]) == is4.end());
  //FIXME:assert(find(is4.begin(),is4.end(),interval_array[3]) != is4.end());
  //FIXME:assert(find(is4.begin(),is4.end(),interval_array[4]) != is4.end());
  

  // -- testing outside intervals ----------------------------------

  is5 = Intervals::intervals_in_range(intervals, 60, 60.1);

  assert(is5.size() == 0);
  assert(find(is5.begin(),is5.end(),interval_array[0]) == is5.end());
  assert(find(is5.begin(),is5.end(),interval_array[1]) == is5.end());
  assert(find(is5.begin(),is5.end(),interval_array[2]) == is5.end());
  assert(find(is5.begin(),is5.end(),interval_array[3]) == is5.end());
  assert(find(is5.begin(),is5.end(),interval_array[4]) == is5.end());

}

int main()
{
  test_Interval();
  test_Intervals();
  test_intervals_in_range();

  return EXIT_SUCCESS;
}
