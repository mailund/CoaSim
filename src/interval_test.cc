
#include "interval.hh"
#include "testing.hh"


static void test_Interval()
{
  // illegal intervals
  try {
    Interval(0.0,0.0);
    ERROR("Intervals cannot be empty.");
  } catch(Interval::empty_interval) {}
  try {
    Interval(0.5,0.0);
    ERROR("Intervals cannot be empty.");
  } catch(Interval::empty_interval) {}
  try {
    Interval(-1.0,1.0);
    ERROR("Intervals should only be in the range [0,1)!");
  } catch(Interval::interval_out_of_range) {}
  try {
    Interval(1.0,1.0);
    ERROR("Intervals should only be in the range [0,1)!");
  } catch(Interval::interval_out_of_range) {}
  try {
    Interval(0.0,1.1);
    ERROR("Intervals should only be in the range [0,1)!");
  } catch(Interval::interval_out_of_range) {}
      

  // these ranges, however, should be legal
  Interval(0.0,0.1);
  Interval(0.0,1.0);

  double start = 0.00, length = 0.50;
  int contacts = 2;

  Interval i(start,length,contacts);

  CHECK(i.start() == start);
  CHECK(i.length() == length);
  CHECK(i.end() == start + length);

  CHECK(i.leaf_contacts() == contacts);

  CHECK(!i.contains_point(-0.10));
  CHECK( i.contains_point( 0.00 - Interval::epsilon)); // included
						       // since start
						       // is closed
  CHECK( i.contains_point( 0.00));
  CHECK( i.contains_point( 0.45));
  CHECK(!i.contains_point( 0.50 + Interval::epsilon)); // not includes
						       // since end is
						       // open
  CHECK(!i.contains_point( 1.00));
  CHECK(!i.contains_point(10.00));


  Interval i2(start,length,contacts);
  Interval i3(start+0.1, length, contacts);
  Interval i4(start, length-0.1, contacts);
  Interval i5(start, length, contacts+3);

  Interval i6(start+0.5+Interval::epsilon, 0.2, contacts);
  Interval i7(start+0.75, 0.2, contacts);

  Interval i8(0.25, 0.2, contacts);
  Interval i9(0.25, 0.25, contacts);

  CHECK( i.overlaps(i2));
  CHECK( i.overlaps(i3));
  CHECK( i.overlaps(i4));
  CHECK( i.overlaps(i5));

  CHECK(!i.overlaps(i6));
  CHECK(!i.overlaps(i7));

  CHECK( i.overlaps(i8));
  CHECK( i.overlaps(i9));

  Interval j = i | i3;
  CHECK(j.is_start(i.start()));
  CHECK(j.is_end(i3.end()));
  CHECK(j.leaf_contacts() == i.leaf_contacts() + i3.leaf_contacts());

  try {
    i | i6;
    ERROR("You cannot join non-overlapping intervals.");
  } catch(Interval::non_overlapping) {}
  try {
    i | i7;
    ERROR("You cannot join non-overlapping intervals.");
  } catch(Interval::non_overlapping) {}

  CHECK(i == i2);
  CHECK(i != i3);
  CHECK(i != i4);
  CHECK(i != i5);
  CHECK(i != i6);
  CHECK(i != i7);
  CHECK(i != i8);
  CHECK(i != i9);

  // i1 < i2 if start1 is smaller than start2, independent of other
  // values.
  CHECK(!(i < i2));
  CHECK( (i < i3));
  CHECK(!(i < i4));
  CHECK(!(i < i5));
  CHECK( (i < i6));
  CHECK( (i < i7));
  CHECK( (i < i8));
  CHECK( (i < i9));
}

static void test_Intervals()	// sorted, non-overlapping intervals
{
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:      |----)
  // 2:             |--)
  // 3:                |----)
  // 4                      |----)
  static double starts[] =  {  0.0,  0.10, 0.25, 0.30, 0.40, };
  static double lengths[] = {  0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.05-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  // --- testing basic functionality ----------------------------------------
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(starts[i]));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(starts[i]+lengths[i]));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.is_end(interval_array[i].end()));


  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.contains_point(starts[i]+lengths[i]/2));
  CHECK(! intervals.contains_point(starts[0] - 2.0));
  // this test only works as long as the last interval's endpoint is
  // the last point
  CHECK(!intervals.contains_point(starts[no_intervals-1]
				  +lengths[no_intervals-1]+2.0));


  //  --- testing copy() ------------------------------------------------
  try {
    intervals.copy(-10,-5);
    ERROR("Interval most be sub-interval of [0.0,1.0)");
  } catch(Intervals::illegal_interval) {}

  try {
    intervals.copy(0,5);
    ERROR("Interval most be sub-interval of [0.0,1.0)");
  } catch(Intervals::illegal_interval) {}

  try {
    intervals.copy(0.5,1.1);
    ERROR("Interval most be sub-interval of [0.0,1.0)");
  } catch(Intervals::illegal_interval) {}

  try {
    intervals.copy(0,0);	// empty interval, but interval 0 starts there
    ERROR("We cannot copy an empty interval!");
  } catch(Intervals::illegal_interval) {}

  try {
    intervals.copy(20,20);	// empty intervals, but 0 and 1 stops here
    ERROR("We cannot copy an empty interval!");
  } catch(Intervals::illegal_interval) {}

  try {
    intervals.copy(40,40);	// empty intervals, middle of 3, start of 4
    ERROR("We cannot copy an empty interval!");
  } catch(Intervals::illegal_interval) {}

  try {
    intervals.copy(1,0);	// the entire interval, but in the wrong order
    ERROR("We cannot copy an inverted interval!");
  } catch(Intervals::illegal_interval) {}


  Intervals cp = intervals.copy(0.0,0.1);
  CHECK(cp.size() == 1);

  cp = intervals.copy(0.0,0.05);
  CHECK(cp.size() == 1);

  cp = intervals.copy(0.015,0.035);
  CHECK(cp.size() == 1);

  cp = intervals.copy(0.05,0.15);
  CHECK(cp.size() == 2);

  cp = intervals.copy(0.22,0.24);
  CHECK(cp.size() == 0);

  cp = intervals.copy(0.22,0.25-Interval::epsilon);
  CHECK(cp.size() == 0);
  cp = intervals.copy(0.22,0.25);
  CHECK(cp.size() == 1);

  cp = intervals.copy(0.20,0.25-Interval::epsilon);
  CHECK(cp.size() == 0);
  cp = intervals.copy(0.20,0.25);
  CHECK(cp.size() == 1);

  cp = intervals.copy(0.25,0.30);
  CHECK(cp.size() == 1);

  cp = intervals.copy(0.25,0.45);
  CHECK(cp.size() == 3);

  cp = intervals.copy(0.25,0.50);
  CHECK(cp.size() == 3);


  intervals.reset();
  CHECK(intervals.size() == 0);
}

static void test_Intervals2()	// reverse sorted intervals
{
  //   .00  .10  .20  .30  .40  .50
  // 0                      |----)
  // 1:                |----)
  // 2:             |--)
  // 3:      |----)
  // 4: |----)
  static double starts[] =  {  0.40, 0.30, 0.25, 0.10, 0.00, };
  static double lengths[] = {  0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.05-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  try {
    for (int i = 0; i < no_intervals; ++i)
      intervals.add(interval_array[i]);
    ERROR("Intervals must be added in the right order!");
  } catch(Intervals::out_of_sequence) {}
}

static void test_Intervals3()	// unsorted intervals
{
  //   .00  .10  .20  .30  .40  .50
  // 0:                |---------|
  // 1                      |----|
  // 2:      |----|
  // 3: |---------|
  // 4:           |----|
  static double starts[] =  { 0.30, 0.40, 0.10, 0.00, 0.20, };
  static double lengths[] = { 0.20, 0.10, 0.10, 0.20, 0.10, };

  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  try {
    for (int i = 0; i < no_intervals; ++i)
      intervals.add(interval_array[i]);
    ERROR("Intervals must be added in the right order!");
  } catch(Intervals::out_of_sequence) {}
}


static void test_Intervals4()	// sorted, touching overlapping intervals
{
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  // 3:                |----)
  // 4                      |----)
  static double starts[] =  {  0.0,  0.10, 0.20, 0.30, 0.40, };
  static double lengths[] = {  0.10, 0.10, 0.10, 0.10, 0.10, };

  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  // --- testing basic functionality ----------------------------------------
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == 1);	// should all be joined into one
}


static void test_Intervals_add_intervals()
{
  // tested in the sum test that just dispatches to add_intervals
}

static void test_Intervals_sum()
{
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:      |----)
  // 2:             |--)
  // 3:                |----)
  // 4                      |----)
  static double starts[] =  {  0.0,  0.10, 0.25, 0.30, 0.40, };
  static double lengths[] = {  0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.05-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  // --- sum with empty intervals ------------------------------------------
  Intervals empty;
  Intervals sum = intervals + empty;
  CHECK(sum.size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_end(interval_array[i].end()));

  sum = empty + intervals;
  CHECK(sum.size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    {
      CHECK(sum.is_start(starts[i]));
      CHECK(sum.is_start(interval_array[i].start()));
    }
  for (int i = 0; i < no_intervals; ++i)
    {
      CHECK(sum.is_end(starts[i]+lengths[i]));
      CHECK(sum.is_end(interval_array[i].end()));
    }


  // --- sum with a gap between the intervals ------------------------------
  //   0.60 0.70 0.80 0.90
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts2[] =  { 0.60, 0.70, 0.80, };
  static double lengths2[] = { 0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array2[] = {
    Interval(starts2[0],lengths2[0]),
    Interval(starts2[1],lengths2[1]),
    Interval(starts2[2],lengths2[2]),
  };
  const int no_intervals2 = (sizeof interval_array2)/sizeof(Interval);

  Intervals intervals2;
  for (int i = 0; i < no_intervals2; ++i)
    intervals2.add(interval_array2[i]);
  CHECK(intervals2.size() == no_intervals2);

  sum = intervals + intervals2;
  CHECK(sum.size() == intervals.size() + intervals2.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum.is_start(interval_array2[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum.is_end(interval_array2[i].end()));

  sum = intervals2 + intervals;
  CHECK(sum.size() == intervals.size() + intervals2.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum.is_start(interval_array2[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum.is_end(interval_array2[i].end()));


  // --- sum with where intervals almost touch in the middle  --------------
  //   0.50 0.60 0.70 0.80
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts3[] =  { 0.50, 0.60, 0.70, };
  static double lengths3[] = { 0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array3[] = {
    Interval(starts3[0],lengths3[0]),
    Interval(starts3[1],lengths3[1]),
    Interval(starts3[2],lengths3[2]),
  };
  const int no_intervals3 = (sizeof interval_array3)/sizeof(Interval);

  Intervals intervals3;
  for (int i = 0; i < no_intervals3; ++i)
    intervals3.add(interval_array3[i]);
  CHECK(intervals3.size() == no_intervals3);

  sum = intervals + intervals3;
  CHECK(sum.size() == intervals.size() + intervals3.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum.is_start(interval_array3[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum.is_end(interval_array3[i].end()));

  sum = intervals3 + intervals;
  CHECK(sum.size() == intervals.size() + intervals3.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum.is_start(interval_array3[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum.is_end(interval_array3[i].end()));

  // --- sum with where intervals touch in the middle  ---------------------
  //   0.50 0.60 0.70 0.80
  // 0:|-----)
  // 1:      |----)
  // 2:           |----)
  static double starts4[] =  { 0.50-Interval::epsilon, 0.60, 0.70, };
  static double lengths4[] = { 0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array4[] = {
    Interval(starts4[0],lengths4[0]),
    Interval(starts4[1],lengths4[1]),
    Interval(starts4[2],lengths4[2]),
  };
  const int no_intervals4 = (sizeof interval_array4)/sizeof(Interval);

  Intervals intervals4;
  for (int i = 0; i < no_intervals4; ++i)
    intervals4.add(interval_array4[i]);
  CHECK(intervals4.size() == no_intervals4);

  sum = intervals + intervals4; // the middle intervals are joined
  CHECK(sum.size() == intervals.size() + intervals4.size() - 1);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 1; i < no_intervals4; ++i)
    CHECK(sum.is_start(interval_array4[i].start()));

  for (int i = 0; i < no_intervals - 1; ++i)
    CHECK(sum.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals4; ++i)
    CHECK(sum.is_end(interval_array4[i].end()));

  sum = intervals4 + intervals;
  CHECK(sum.size() == intervals.size() + intervals4.size() - 1);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.is_start(interval_array[i].start()));
  for (int i = 1; i < no_intervals4; ++i)
    CHECK(sum.is_start(interval_array4[i].start()));

  for (int i = 0; i < no_intervals - 1; ++i)
    CHECK(sum.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals4; ++i)
    CHECK(sum.is_end(interval_array4[i].end()));

}

static void test_Intervals_merge()
{
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:      |----)
  // 2:             |--)
  // 3:                |----)
  // 4                      |----)
  static double starts[] =  {  0.0,  0.10, 0.25, 0.30, 0.40, };
  static double lengths[] = {  0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.05-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };
  
  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  
  // --- sum with empty intervals ------------------------------------------
  Intervals empty;
  Intervals merge = intervals | empty;
  CHECK(merge.size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));

  merge = empty | intervals;
  CHECK(merge.size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));

  
  // --- merge with a gap between the intervals ------------------------------
  //   0.60 0.70 0.80 0.90
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts2[] =  { 0.60, 0.70, 0.80, };
  static double lengths2[] = { 0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array2[] = {
    Interval(starts2[0],lengths2[0]),
    Interval(starts2[1],lengths2[1]),
    Interval(starts2[2],lengths2[2]),
  };
  const int no_intervals2 = (sizeof interval_array2)/sizeof(Interval);

  Intervals intervals2;
  for (int i = 0; i < no_intervals2; ++i)
    intervals2.add(interval_array2[i]);
  CHECK(intervals2.size() == no_intervals2);

  merge = intervals | intervals2;
  CHECK(merge.size() == intervals.size() + intervals2.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(merge.is_start(interval_array2[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(merge.is_end(interval_array2[i].end()));

  merge = intervals2 | intervals;
  CHECK(merge.size() == intervals.size() + intervals2.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(merge.is_start(interval_array2[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(merge.is_end(interval_array2[i].end()));


  // --- merge with where intervals almost touch in the middle  --------------
  //   0.50 0.60 0.70 0.80
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts3[] =  { 0.50, 0.60, 0.70, };
  static double lengths3[] = { 0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array3[] = {
    Interval(starts3[0],lengths3[0]),
    Interval(starts3[1],lengths3[1]),
    Interval(starts3[2],lengths3[2]),
  };
  const int no_intervals3 = (sizeof interval_array3)/sizeof(Interval);

  Intervals intervals3;
  for (int i = 0; i < no_intervals3; ++i)
    intervals3.add(interval_array3[i]);
  CHECK(intervals3.size() == no_intervals3);

  merge = intervals | intervals3;
  CHECK(merge.size() == intervals.size() + intervals3.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(merge.is_start(interval_array3[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(merge.is_end(interval_array3[i].end()));

  merge = intervals3 | intervals;
  CHECK(merge.size() == intervals.size() + intervals3.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(merge.is_start(interval_array3[i].start()));

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(merge.is_end(interval_array3[i].end()));

  // --- merge with where intervals touch in the middle  ---------------------
  //   0.50 0.60 0.70 0.80
  // 0:|-----)
  // 1:      |----)
  // 2:           |----)
  static double starts4[] =  { 0.50-Interval::epsilon, 0.60, 0.70, };
  static double lengths4[] = { 0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon,
			       0.10-1.5*Interval::epsilon, };

  Interval interval_array4[] = {
    Interval(starts4[0],lengths4[0]),
    Interval(starts4[1],lengths4[1]),
    Interval(starts4[2],lengths4[2]),
  };
  const int no_intervals4 = (sizeof interval_array4)/sizeof(Interval);

  Intervals intervals4;
  for (int i = 0; i < no_intervals4; ++i)
    intervals4.add(interval_array4[i]);
  CHECK(intervals4.size() == no_intervals4);

  merge = intervals | intervals4; // the middle intervals are joined
  CHECK(merge.size() == intervals.size() + intervals4.size() - 1);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 1; i < no_intervals4; ++i)
    CHECK(merge.is_start(interval_array4[i].start()));

  for (int i = 0; i < no_intervals - 1; ++i)
    CHECK(merge.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals4; ++i)
    CHECK(merge.is_end(interval_array4[i].end()));

  merge = intervals4 | intervals;
  CHECK(merge.size() == intervals.size() + intervals4.size() - 1);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 1; i < no_intervals4; ++i)
    CHECK(merge.is_start(interval_array4[i].start()));

  for (int i = 0; i < no_intervals - 1; ++i)
    CHECK(merge.is_end(interval_array[i].end()));
  for (int i = 0; i < no_intervals4; ++i)
    CHECK(merge.is_end(interval_array4[i].end()));


  // --- merging two intervals that are actually merged
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:             |--)
  // 2                      |----)
  static double starts5[] =  {  0.0,  0.25, 0.40, };
  static double lengths5[] = {  0.10-1.5*Interval::epsilon,
				0.05-1.5*Interval::epsilon,
				0.10-1.5*Interval::epsilon, };

  Interval interval_array5[] = {
    Interval(starts5[0],lengths5[0]),
    Interval(starts5[1],lengths5[1]),
    Interval(starts5[2],lengths5[2]),
  };
  const int no_intervals5 = (sizeof interval_array5)/sizeof(Interval);

  Intervals intervals5;
  for (int i = 0; i < no_intervals5; ++i)
    intervals5.add(interval_array5[i]);

  //   .00  .10  .20  .30  .40  .50
  // 0:      |----)
  // 1:                |----)
  static double starts6[] =  {  0.10, 0.30, };
  static double lengths6[] = {  0.10-1.5*Interval::epsilon,
				0.10-1.5*Interval::epsilon, };

  Interval interval_array6[] = {
    Interval(starts6[0],lengths6[0]),
    Interval(starts6[1],lengths6[1]),
  };
  const int no_intervals6 = (sizeof interval_array6)/sizeof(Interval);

  Intervals intervals6;
  for (int i = 0; i < no_intervals6; ++i)
    intervals6.add(interval_array6[i]);
 

  // the merge should give us the same intervals as the first interval
  merge = intervals5 | intervals6;
  CHECK(merge.size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));

  merge = intervals6 | intervals5;
  CHECK(merge.size() == intervals.size());

  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_start(interval_array[i].start()));
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.is_end(interval_array[i].end()));



  //   .00  .10  .20  .30  .40  .50
  // 0:         |--------)
  static double starts7[] =  {  0.15, };
  static double lengths7[] = {  0.20-1.5*Interval::epsilon, };

  Interval interval_array7[] = {
    Interval(starts7[0],lengths7[0]),
  };
  const int no_intervals7 = (sizeof interval_array7)/sizeof(Interval);

  Intervals intervals7;
  for (int i = 0; i < no_intervals7; ++i)
    intervals7.add(interval_array7[i]);

  // the merge of 6 and 7 should give one interval, spanning from 0.10
  // to 0.40-1.5*epsilon

  merge = intervals6 | intervals7;
  CHECK(merge.size() == 1);
  CHECK(merge.is_start(0.10));
  CHECK(merge.is_end(0.40-1.5*Interval::epsilon));

  merge = intervals7 | intervals6;
  CHECK(merge.size() == 1);
  CHECK(merge.is_start(0.10));
  CHECK(merge.is_end(0.40-1.5*Interval::epsilon));
}



static void test_intervals_in_range()
{
  return; // FIXME: refactor this test when the method has been
	  // re-factored

  //   0.00 0.10 0.20 0.30 0.40 0.50
  // 0: |---------|
  // 1:      |----|
  // 2:           |----|
  // 3:                |---------|
  // 4                      |----|
  static double starts[] =  { 0.00, 0.10, 0.20, 0.30, 0.40, };
  static double lengths[] = { 0.20, 0.10, 0.10, 0.20, 0.10, };

  Interval interval_array[] = {
    Interval(starts[0],lengths[0]),
    Interval(starts[1],lengths[1]),
    Interval(starts[2],lengths[2]),
    Interval(starts[3],lengths[3]),
    Interval(starts[4],lengths[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  std::vector< Interval > intervals(interval_array, interval_array+no_intervals);
  std::vector< Interval > is1, is2, is3, is4, is5;

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
  test_Intervals4();

  //test_Intervals_add_intervals();
  test_Intervals_sum();
  test_Intervals_merge();

  test_intervals_in_range();

  REPORT_RESULTS;
}
