
#include "interval.hh"
#include "testing.hh"

static void test_Interval()
{
  double start = 0.0, length = 10.0;
  int contacts = 2;

  Interval i(start,length,contacts);

  CHECK(i.start() == start);
  CHECK(i.length() == length);
  CHECK(i.end() == start + length);

  start = 10.0; i.set_start(start);
  CHECK(i.start() == start);
  CHECK(i.length() == length);
  CHECK(i.end() == start + length);

  length = 20.3; i.set_length(length);
  CHECK(i.start() == start);
  CHECK(i.length() == length);
  CHECK(i.end() == start + length);

  CHECK(i.leaf_contacts() == contacts);
  contacts = 4; i.set_leaf_contacts(contacts);
  CHECK(i.leaf_contacts() == contacts);
}

static void test_Intervals()	// sorted intervals
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
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  // --- testing basic functionality ----------------------------------------
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  for (int i = 0; i < no_intervals; ++i)
    {
      //FIXME CHECK(&intervals[i] == interval_array[i]);
      //FIXME CHECK(&intervals.interval(i) == interval_array[i]);
    }

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(starts[i]));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(starts[i]+lengths[i]));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(interval_array[i]->end()));


  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.contains_point(starts[i]+lengths[i]/2));
  CHECK(! intervals.contains_point(starts[0] - 2.0));
  // this test only works as long as the last interval's endpoint is
  // the last point
  CHECK(!intervals.contains_point(starts[no_intervals-1]
				   +lengths[no_intervals-1]+2.0));


  intervals.remove(2);
  CHECK(intervals.size() == no_intervals - 1);
  // this test only works as long as other intervals doesn't overlap
  // interval 2
  CHECK(! intervals.contains_point(starts[2] + lengths[2]/2));

  //  --- testing copy() ------------------------------------------------
  Intervals *cp;
  cp = intervals.copy(-10,-5);
  CHECK(cp->size() == 0);

  cp = intervals.copy(0,0);	// empty interval, but interval 0 starts there
  CHECK(cp->size() == 1);

  cp = intervals.copy(11,19);
  CHECK(cp->size() == 2);

  cp = intervals.copy(20,20);	// empty intervals, but 0 and 1 stops here
  CHECK(cp->size() == 2);

  cp = intervals.copy(21,29);	// deleted interval 2
  CHECK(cp->size() == 0);

  cp = intervals.copy(40,40);	// empty intervals, middle of 3, start of 4
  CHECK(cp->size() == 2);

  cp = intervals.copy(39,41);
  CHECK(cp->size() == 2);


  intervals.reset();
  CHECK(intervals.size() == 0);
}

static void test_Intervals2()	// reverse sorted intervals
{
  //    00   10   20   30   40   50
  // 0                      |----|
  // 1:                |---------|
  // 2:           |----|
  // 3:      |----|
  // 4: |---------|
  static double starts[] =  { 40, 30, 20, 10,  0, };
  static double lengths[] = { 10, 20, 10, 10, 20, };

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
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  // --- testing basic functionality ----------------------------------------
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  for (int i = 0; i < no_intervals; ++i)
    {
      //FIXME CHECK(&intervals[i] == interval_array[i]);
      //FIXME CHECK(&intervals.interval(i) == interval_array[i]);
    }

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(starts[i]));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(starts[i]+lengths[i]));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(interval_array[i]->end()));


  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.contains_point(starts[i]+lengths[i]/2));
  CHECK(! intervals.contains_point(starts[0] - 2.0));
  // this test only works as long as the last interval's endpoint is
  // the last point
  CHECK(!intervals.contains_point(starts[no_intervals-1]
				   +lengths[no_intervals-1]+2.0));


  intervals.remove(2);
  CHECK(intervals.size() == no_intervals - 1);
  // this test only works as long as other intervals doesn't overlap
  // interval 2
  CHECK(! intervals.contains_point(starts[2] + lengths[2]/2));

  //  --- testing copy() ------------------------------------------------
  Intervals *cp;
  cp = intervals.copy(-10,-5);
  CHECK(cp->size() == 0);

  cp = intervals.copy(0,0);	// empty interval, but interval 0 starts there
  CHECK(cp->size() == 1);

  cp = intervals.copy(11,19);
  CHECK(cp->size() == 2);

  cp = intervals.copy(20,20);	// empty intervals, but 0 and 1 stops here
  CHECK(cp->size() == 2);

  cp = intervals.copy(21,29);	// deleted interval 2
  CHECK(cp->size() == 0);

  cp = intervals.copy(40,40);	// empty intervals, middle of 3, start of 4
  CHECK(cp->size() == 2);

  cp = intervals.copy(39,41);
  CHECK(cp->size() == 2);


  intervals.reset();
  CHECK(intervals.size() == 0);
}

static void test_Intervals3()	// unsorted intervals
{
  //    00   10   20   30   40   50
  // 0:                |---------|
  // 1                      |----|
  // 2:      |----|
  // 3: |---------|
  // 4:           |----|
  static double starts[] =  { 30, 40, 10,  0, 20, };
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
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  // --- testing basic functionality ----------------------------------------
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  for (int i = 0; i < no_intervals; ++i)
    {
      //FIXME CHECK(&intervals[i] == interval_array[i]);
      //FIXME CHECK(&intervals.interval(i) == interval_array[i]);
    }

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(starts[i]));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(starts[i]+lengths[i]));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(interval_array[i]->end()));


  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.contains_point(starts[i]+lengths[i]/2));
  CHECK(! intervals.contains_point(starts[0] - 2.0));
  // this test only works as long as the last interval's endpoint is
  // the last point
  CHECK(!intervals.contains_point(starts[no_intervals-1]
				   +lengths[no_intervals-1]+2.0));


  intervals.remove(2);
  CHECK(intervals.size() == no_intervals - 1);
  // this test only works as long as other intervals doesn't overlap
  // interval 2
  CHECK(! intervals.contains_point(starts[2] + lengths[2]/2));

  //  --- testing copy() ------------------------------------------------
  Intervals *cp;
  cp = intervals.copy(-10,-5);
  CHECK(cp->size() == 0);

  cp = intervals.copy(0,0);	// empty interval, but interval 0 starts there
  CHECK(cp->size() == 1);

  cp = intervals.copy(11,19);
  CHECK(cp->size() == 2);

  cp = intervals.copy(20,20);	// empty intervals, but 0 and 1 stops here
  CHECK(cp->size() == 2);

  cp = intervals.copy(21,29);	// deleted interval 2
  CHECK(cp->size() == 0);

  cp = intervals.copy(40,40);	// empty intervals, middle of 3, start of 4
  CHECK(cp->size() == 2);

  cp = intervals.copy(39,41);
  CHECK(cp->size() == 2);


  intervals.reset();
  CHECK(intervals.size() == 0);
}


static void test_Intervals_sum()
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
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  // --- sum with empty intervals ------------------------------------------
  Intervals empty;
  Intervals *sum = intervals + empty;
  CHECK(sum->size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_end(interval_array[i]->end()));

  sum = empty + intervals;
  CHECK(sum->size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    {
      CHECK(sum->is_start(starts[i]));
      CHECK(sum->is_start(interval_array[i]->start()));
    }
  for (int i = 0; i < no_intervals; ++i)
    {
      CHECK(sum->is_end(starts[i]+lengths[i]));
      CHECK(sum->is_end(interval_array[i]->end()));
    }


  // --- sum with a gap between the intervals ------------------------------
  //    60   70   80   90
  // 0: |---------|
  // 1:      |----|
  // 2:           |----|
  static double starts2[] =  { 60, 70, 80, };
  static double lengths2[] = { 20, 10, 10, };

  Interval* interval_array2[] = {
    new Interval(starts2[0],lengths2[0]),
    new Interval(starts2[1],lengths2[1]),
    new Interval(starts2[2],lengths2[2]),
  };
  const int no_intervals2 = (sizeof interval_array2)/sizeof(Interval*);

  Intervals intervals2;
  for (int i = 0; i < no_intervals2; ++i)
    intervals2.add(interval_array2[i]);
  CHECK(intervals2.size() == no_intervals2);

  delete sum;
  sum = intervals + intervals2;
  CHECK(sum->size() == intervals.size() + intervals2.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum->is_start(interval_array2[i]->start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_end(interval_array[i]->end()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum->is_end(interval_array2[i]->end()));

  delete sum;
  sum = intervals2 + intervals;
  CHECK(sum->size() == intervals.size() + intervals2.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum->is_start(interval_array2[i]->start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_end(interval_array[i]->end()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum->is_end(interval_array2[i]->end()));


  // --- sum with where intervals touch in the middle  ---------------------
  //    50   60   70   80
  // 0: |---------|
  // 1:      |----|
  // 2:           |----|
  static double starts3[] =  { 50, 60, 70, };
  static double lengths3[] = { 20, 10, 10, };

  Interval* interval_array3[] = {
    new Interval(starts3[0],lengths3[0]),
    new Interval(starts3[1],lengths3[1]),
    new Interval(starts3[2],lengths3[2]),
  };
  const int no_intervals3 = (sizeof interval_array3)/sizeof(Interval*);

  Intervals intervals3;
  for (int i = 0; i < no_intervals3; ++i)
    intervals3.add(interval_array3[i]);
  CHECK(intervals3.size() == no_intervals3);

  delete sum;
  sum = intervals + intervals3;
  CHECK(sum->size() == intervals.size() + intervals3.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum->is_start(interval_array3[i]->start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_end(interval_array[i]->end()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum->is_end(interval_array3[i]->end()));

  delete sum;
  sum = intervals3 + intervals;
  CHECK(sum->size() == intervals.size() + intervals3.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_start(interval_array[i]->start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum->is_start(interval_array3[i]->start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum->is_end(interval_array[i]->end()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum->is_end(interval_array3[i]->end()));
}

static void test_Intervals_merge()
{
  // FIXME: merge
}

static void test_Intervals_add_interval()
{
  // FIXME: add_interval
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

  Interval* interval_array[] = {
    new Interval(starts[0],lengths[0]),
    new Interval(starts[1],lengths[1]),
    new Interval(starts[2],lengths[2]),
    new Interval(starts[3],lengths[3]),
    new Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  // FIXME: CHECK GC here -- are the intervals copied or are new
  // intervals allocated?
  std::vector< Interval* > intervals(interval_array, interval_array+no_intervals);
  std::vector< Interval* > is1, is2, is3, is4, is5;

  // -- testing middle of intervals --------------------------------

  is1 = Intervals::intervals_in_range(intervals, 11, 19);

  CHECK(is1.size() == 2);
  CHECK(find(is1.begin(),is1.end(),interval_array[0]) != is1.end());
  CHECK(find(is1.begin(),is1.end(),interval_array[1]) != is1.end());
  CHECK(find(is1.begin(),is1.end(),interval_array[2]) == is1.end());
  CHECK(find(is1.begin(),is1.end(),interval_array[3]) == is1.end());
  CHECK(find(is1.begin(),is1.end(),interval_array[4]) == is1.end());

  is2 = Intervals::intervals_in_range(intervals, 21, 24);

  CHECK(is2.size() == 1);
  CHECK(find(is2.begin(),is2.end(),interval_array[0]) == is2.end());
  CHECK(find(is2.begin(),is2.end(),interval_array[1]) == is2.end());
  CHECK(find(is2.begin(),is2.end(),interval_array[2]) != is2.end());
  CHECK(find(is2.begin(),is2.end(),interval_array[3]) == is2.end());
  CHECK(find(is2.begin(),is2.end(),interval_array[4]) == is2.end());

  // -- testing end of intervals -----------------------------------
  // FIXME: I'm not completly sure about this one, should the
  // end-points be considered in the interval?  in other words, which
  // ends of the interval are open and which are closed?  Here I've
  // just assumed that both end-points are closed.

  is3 = Intervals::intervals_in_range(intervals, 0, 0.1);

  CHECK(is3.size() == 1);
  CHECK(find(is3.begin(),is3.end(),interval_array[0]) != is3.end());
  CHECK(find(is3.begin(),is3.end(),interval_array[1]) == is3.end());
  CHECK(find(is3.begin(),is3.end(),interval_array[2]) == is3.end());
  CHECK(find(is3.begin(),is3.end(),interval_array[3]) == is3.end());
  CHECK(find(is3.begin(),is3.end(),interval_array[4]) == is3.end());

  is4 = Intervals::intervals_in_range(intervals, 50, 50.1);
  
  CHECK(is4.size() == 2);
  CHECK(find(is4.begin(),is4.end(),interval_array[0]) == is4.end());
  CHECK(find(is4.begin(),is4.end(),interval_array[1]) == is4.end());
  CHECK(find(is4.begin(),is4.end(),interval_array[2]) == is4.end());
  CHECK(find(is4.begin(),is4.end(),interval_array[3]) != is4.end());
  CHECK(find(is4.begin(),is4.end(),interval_array[4]) != is4.end());
  

  // -- testing outside intervals ----------------------------------

  is5 = Intervals::intervals_in_range(intervals, 60, 60.1);

  CHECK(is5.size() == 0);
  CHECK(find(is5.begin(),is5.end(),interval_array[0]) == is5.end());
  CHECK(find(is5.begin(),is5.end(),interval_array[1]) == is5.end());
  CHECK(find(is5.begin(),is5.end(),interval_array[2]) == is5.end());
  CHECK(find(is5.begin(),is5.end(),interval_array[3]) == is5.end());
  CHECK(find(is5.begin(),is5.end(),interval_array[4]) == is5.end());

}

int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  test_Interval();

  test_Intervals();
  test_Intervals2();
  test_Intervals3();

  test_Intervals_sum();
  test_Intervals_merge();
  test_Intervals_add_interval();

  test_intervals_in_range();

  REPORT_RESULTS;
}
