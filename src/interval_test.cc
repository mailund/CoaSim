
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
    Interval(0.5,0.5);
    ERROR("Intervals cannot be empty.");
  } catch(Interval::empty_interval) {}
  try {
    Interval(-1.0,1.0);
    ERROR("Intervals should only be in the range [0,1)!");
  } catch(Interval::interval_out_of_range) {}
  try {
    Interval(1.0,1.1);
    ERROR("Intervals should only be in the range [0,1)!");
  } catch(Interval::interval_out_of_range) {}
  try {
    Interval(0.0,1.1);
    ERROR("Intervals should only be in the range [0,1)!");
  } catch(Interval::interval_out_of_range) {}
  try {
    Interval(0.5,0.1);
    ERROR("Intervals cannot be inverted!");
  } catch(Interval::empty_interval) {}
      

  // these ranges, however, should be legal
  Interval(0.0,0.1);
  Interval(0.0,1.0);

  double start = 0.00, end = 0.50;
  unsigned int contacts = 2;

  Interval i(start,end,contacts);

  CHECK(i.start() == start);
  CHECK(i.end() == end);
  CHECK(i.length() == end-start);

  CHECK(i.leaf_contacts() == contacts);

  CHECK(!i.contains_point(-0.10));
  CHECK( i.contains_point( 0.00)); // included since start is closed
  CHECK( i.contains_point( 0.00));
  CHECK( i.contains_point( 0.45));
  CHECK(!i.contains_point( 0.50)); // not includes since end is open
  CHECK(!i.contains_point( 1.00));
  CHECK(!i.contains_point(10.00));

  Interval i2(start,     end,     contacts);
  Interval i3(start+0.1, end+0.1, contacts);
  Interval i4(start,     end-0.1, contacts);
  Interval i5(start,     end,     contacts+3);

  Interval i6(start+0.5,  0.7,  contacts);
  Interval i7(start+0.75, 0.95, contacts);

  Interval i8(0.25, 0.45, contacts);
  Interval i9(0.25, 0.50, contacts);

  CHECK( i.overlaps(i2));
  CHECK( i.overlaps(i3));
  CHECK( i.overlaps(i4));
  CHECK( i.overlaps(i5));

  CHECK(!i.overlaps(i6));
  CHECK(!i.overlaps(i7));

  CHECK( i.overlaps(i8));
  CHECK( i.overlaps(i9));

  CHECK(i == i2);
  CHECK(i != i3);
  CHECK(i != i4);
  CHECK(i != i5);
  CHECK(i != i6);
  CHECK(i != i7);
  CHECK(i != i8);
  CHECK(i != i9);

#if 0
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
#endif
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
  static double ends[]   =  {  0.10, 0.20, 0.30, 0.40, 0.50, };

  Interval interval_array[] = {
    Interval(starts[0],ends[0]),
    Interval(starts[1],ends[1]),
    Interval(starts[2],ends[2]),
    Interval(starts[3],ends[3]),
    Interval(starts[4],ends[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  // --- testing basic functionality ----------------------------------------
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  CHECK(intervals.size() == no_intervals);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals[i].start() == starts[i]);
  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals[i].end() == ends[i]);

  for (int i = 0; i < no_intervals; ++i)
    CHECK(intervals.contains_point(starts[i]+(ends[i]-starts[i])/2));

  try {
    intervals.contains_point(starts[0] - 0.2);
    ERROR("Accessing points before 0.");
  } catch (std::out_of_range) {}

  // this test only works as long as the last interval's endpoint is
  // the last point
  CHECK(!intervals.contains_point(ends[no_intervals-1]+0.1));


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

#if 0 // no longer considered an error to copy an empty interval
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
#endif

  try {
    intervals.copy(1,0);	// the entire interval, but in the wrong order
    ERROR("We cannot copy an inverted interval!");
  } catch(Intervals::illegal_interval) {}

  Intervals cp = intervals.copy(0.0,0.1);
  CHECK(cp.size() == 1);
  CHECK(cp.first_point() == 0.0);
  CHECK(cp.last_point()  == 0.1);

  cp = intervals.copy(0.0,0.05);
  CHECK(cp.size() == 1);
  CHECK(cp.first_point() == 0.0);
  CHECK(cp.last_point()  == 0.05);

  cp = intervals.copy(0.015,0.035);
  CHECK(cp.size() == 1);
  CHECK(cp.first_point() == 0.015);
  CHECK(cp.last_point()  == 0.035);

  cp = intervals.copy(0.05,0.15);
  CHECK(cp.size() == 2);
  CHECK(cp.first_point() == 0.05);
  CHECK(cp.last_point()  == 0.15);

  cp = intervals.copy(0.22,0.24);
  CHECK(cp.size() == 0);

  cp = intervals.copy(0.22,0.25);
  CHECK(cp.size() == 0);

  cp = intervals.copy(0.20,0.25);
  CHECK(cp.size() == 0);

  cp = intervals.copy(0.25,0.30);
  CHECK(cp.size() == 1);
  CHECK(cp.first_point() == 0.25);
  CHECK(cp.last_point()  == 0.30);

  cp = intervals.copy(0.25,0.45);
  CHECK(cp.size() == 3);
  CHECK(cp.first_point() == 0.25);
  CHECK(cp.last_point()  == 0.45);

  cp = intervals.copy(0.25,0.50);
  CHECK(cp.size() == 3);
  CHECK(cp.first_point() == 0.25);
  CHECK(cp.last_point()  == 0.50);


  // try copying from before and after an interval

  //   0.60 0.70 0.80 0.90
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts2[] = { 0.60, 0.70, 0.80, };
  static double ends2[]   = { 0.70, 0.80, 0.90, };

  Interval interval_array2[] = {
    Interval(starts2[0],ends2[0]),
    Interval(starts2[1],ends2[1]),
    Interval(starts2[2],ends2[2]),
  };
  const int no_intervals2 = (sizeof interval_array2)/sizeof(Interval);

  Intervals intervals2;
  for (int i = 0; i < no_intervals2; ++i)
    intervals2.add(interval_array2[i]);

  cp = intervals2.copy(0.00,0.65);
  CHECK(cp.size() == 1);

  cp = intervals2.copy(0.85,0.95);
  CHECK(cp.size() == 1);
}

static void test_Intervals2()	// reverse sorted intervals
{
  //   .00  .10  .20  .30  .40  .50
  // 0                      |----)
  // 1:                |----)
  // 2:             |--)
  // 3:      |----)
  // 4: |----)
  static double starts[] =  { 0.40, 0.30, 0.25, 0.10, 0.00, };
  static double ends[] =    { 0.50, 0.40, 0.30, 0.20, 0.10, };

  Interval interval_array[] = {
    Interval(starts[0],ends[0]),
    Interval(starts[1],ends[1]),
    Interval(starts[2],ends[2]),
    Interval(starts[3],ends[3]),
    Interval(starts[4],ends[4]),
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
  // 0:                |----)
  // 1                      |----)
  // 2:      |----)
  // 3: |----)
  // 4:           |----)
  static double starts[] =  { 0.30, 0.40, 0.10, 0.00, 0.20, };
  static double ends[] =    { 0.40, 0.50, 0.20, 0.10, 0.30, };

  Interval interval_array[] = {
    Interval(starts[0],ends[0]),
    Interval(starts[1],ends[1]),
    Interval(starts[2],ends[2]),
    Interval(starts[3],ends[3]),
    Interval(starts[4],ends[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  try {
    for (int i = 0; i < no_intervals; ++i)
      intervals.add(interval_array[i]);
    ERROR("Intervals must be added in the right order!");
  } catch(Intervals::out_of_sequence) {}
}


static void test_Intervals4()	// sorted, but overlapping intervals
{
  //   .00  .10  .20  .30  .40  .50
  // 0: |----0)
  // 1:      |-----)
  // 2:           |-----)
  // 3:                |-----)
  // 4                      |----)
  static double starts[] = {  0.0,  0.10, 0.20, 0.30, 0.40, };
  static double ends[]   = { 0.11, 0.21, 0.31, 0.41, 0.50, };

  Interval interval_array[] = {
    Interval(starts[0],ends[0]),
    Interval(starts[1],ends[1]),
    Interval(starts[2],ends[2]),
    Interval(starts[3],ends[3]),
    Interval(starts[4],ends[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  // --- testing basic functionality ----------------------------------------
  try {
  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  ERROR("Intervals must be non-overlapping!");
  } catch(Intervals::out_of_sequence) {}
}



static void test_Intervals_sum()
{
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:      |----)
  // 2:             |--)
  // 3:                |----)
  // 4                      |----)
  static double starts[] = { 0.00, 0.10, 0.25, 0.30, 0.40, };
  static double ends[]   = { 0.10, 0.20, 0.30, 0.40, 0.50, };

  Interval interval_array[] = {
    Interval(starts[0],ends[0]),
    Interval(starts[1],ends[1]),
    Interval(starts[2],ends[2]),
    Interval(starts[3],ends[3]),
    Interval(starts[4],ends[4]),
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
    CHECK(sum.contains_point(interval_array[i].start()));

  sum = empty + intervals;
  CHECK(sum.size() == intervals.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.contains_point(interval_array[i].start()));


  // --- sum with a gap between the intervals ------------------------------
  //   0.60 0.70 0.80 0.90
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts2[] = { 0.60, 0.70, 0.80, };
  static double ends2[]   = { 0.70, 0.80, 0.90, };

  Interval interval_array2[] = {
    Interval(starts2[0],ends2[0]),
    Interval(starts2[1],ends2[1]),
    Interval(starts2[2],ends2[2]),
  };
  const int no_intervals2 = (sizeof interval_array2)/sizeof(Interval);

  Intervals intervals2;
  for (int i = 0; i < no_intervals2; ++i)
    intervals2.add(interval_array2[i]);
  CHECK(intervals2.size() == no_intervals2);

  sum = intervals + intervals2;
  CHECK(sum.size() == intervals.size() + intervals2.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum.contains_point(interval_array2[i].start()));

  sum = intervals2 + intervals;
  CHECK(sum.size() == intervals.size() + intervals2.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(sum.contains_point(interval_array2[i].start()));


  // --- sum with where intervals almost touch in the middle  --------------
  //   0.50 0.60 0.70 0.80
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts3[] = { 0.50, 0.60, 0.70, };
  static double ends3[]   = { 0.60, 0.70, 0.80, };

  Interval interval_array3[] = {
    Interval(starts3[0],ends3[0]),
    Interval(starts3[1],ends3[1]),
    Interval(starts3[2],ends3[2]),
  };
  const int no_intervals3 = (sizeof interval_array3)/sizeof(Interval);

  Intervals intervals3;
  for (int i = 0; i < no_intervals3; ++i)
    intervals3.add(interval_array3[i]);
  CHECK(intervals3.size() == no_intervals3);

  sum = intervals + intervals3;
  CHECK(sum.size() == intervals.size() + intervals3.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum.contains_point(interval_array3[i].start()));

  sum = intervals3 + intervals;
  CHECK(sum.size() == intervals.size() + intervals3.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(sum.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(sum.contains_point(interval_array3[i].start()));

  // --- sum with where intervals touch in the middle  ---------------------
  //    0.50 0.60 0.70 0.80
  // 0:|------)
  // 1:       |----)
  // 2:            |----)
  static double starts4[] = { 0.49, 0.60, 0.70, };
  static double ends4[]   = { 0.60, 0.70, 0.80, };

  Interval interval_array4[] = {
    Interval(starts4[0],ends4[0]),
    Interval(starts4[1],ends4[1]),
    Interval(starts4[2],ends4[2]),
  };
  const int no_intervals4 = (sizeof interval_array4)/sizeof(Interval);

  Intervals intervals4;
  for (int i = 0; i < no_intervals4; ++i)
    intervals4.add(interval_array4[i]);
  CHECK(intervals4.size() == no_intervals4);

  try {
    intervals + intervals4;
    ERROR("intervals out of sequence");
  } catch (Intervals::out_of_sequence) {}

  try {
    intervals4 + intervals;
    ERROR("intervals out of sequence");
  } catch (Intervals::out_of_sequence) {}
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
  static double ends[] = {  0.10, 0.20, 0.30, 0.40, 0.50, };
  
  Interval interval_array[] = {
    Interval(starts[0],ends[0]),
    Interval(starts[1],ends[1]),
    Interval(starts[2],ends[2]),
    Interval(starts[3],ends[3]),
    Interval(starts[4],ends[4]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval);

  Intervals intervals;
  for (int i = 0; i < no_intervals; ++i)
    intervals.add(interval_array[i]);
  
  // --- merge with empty intervals ------------------------------------------
  Intervals empty;
  Intervals merge = intervals | empty;
  CHECK(merge.size() == intervals.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));

  merge = empty | intervals;
  CHECK(merge.size() == intervals.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));

  
  // --- merge with a gap between the intervals ------------------------------
  //   0.60 0.70 0.80 0.90
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts2[] = { 0.60, 0.70, 0.80, };
  static double ends2[]   = { 0.70, 0.80, 0.90, };

  Interval interval_array2[] = {
    Interval(starts2[0],ends2[0]),
    Interval(starts2[1],ends2[1]),
    Interval(starts2[2],ends2[2]),
  };
  const int no_intervals2 = (sizeof interval_array2)/sizeof(Interval);

  Intervals intervals2;
  for (int i = 0; i < no_intervals2; ++i)
    intervals2.add(interval_array2[i]);
  CHECK(intervals2.size() == no_intervals2);

  merge = intervals | intervals2;
  CHECK(merge.size() == intervals.size() + intervals2.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(merge.contains_point(interval_array2[i].start()));

  merge = intervals2 | intervals;
  CHECK(merge.size() == intervals.size() + intervals2.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals2; ++i)
    CHECK(merge.contains_point(interval_array2[i].start()));


  // --- merge with where intervals almost touch in the middle  --------------
  //   0.50 0.60 0.70 0.80
  // 0: |----)
  // 1:      |----)
  // 2:           |----)
  static double starts3[] = { 0.50, 0.60, 0.70, };
  static double ends3[]   = { 0.60, 0.70, 0.80, };

  Interval interval_array3[] = {
    Interval(starts3[0],ends3[0]),
    Interval(starts3[1],ends3[1]),
    Interval(starts3[2],ends3[2]),
  };
  const int no_intervals3 = (sizeof interval_array3)/sizeof(Interval);

  Intervals intervals3;
  for (int i = 0; i < no_intervals3; ++i)
    intervals3.add(interval_array3[i]);
  CHECK(intervals3.size() == no_intervals3);

  merge = intervals | intervals3;
  CHECK(merge.size() == intervals.size() + intervals3.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(merge.contains_point(interval_array3[i].start()));


  merge = intervals3 | intervals;
  CHECK(merge.size() == intervals.size() + intervals3.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals3; ++i)
    CHECK(merge.contains_point(interval_array3[i].start()));

  // --- merge with where intervals touch in the middle  ---------------------
  //   0.50 0.60 0.70 0.80
  // 0:|-----)
  // 1:      |----)
  // 2:           |----)
  static double starts4[] = { 0.50, 0.60, 0.70, };
  static double ends4[]   = { 0.60, 0.70, 0.80, };

  Interval interval_array4[] = {
    Interval(starts4[0],ends4[0]),
    Interval(starts4[1],ends4[1]),
    Interval(starts4[2],ends4[2]),
  };
  const int no_intervals4 = (sizeof interval_array4)/sizeof(Interval);

  Intervals intervals4;
  for (int i = 0; i < no_intervals4; ++i)
    intervals4.add(interval_array4[i]);
  CHECK(intervals4.size() == no_intervals4);

  merge = intervals | intervals4;
  CHECK(merge.size() == intervals.size() + intervals4.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals4; ++i)
    CHECK(merge.contains_point(interval_array4[i].start()));

  merge = intervals4 | intervals;
  CHECK(merge.size() == intervals.size() + intervals4.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));
  for (int i = 0; i < no_intervals4; ++i)
    CHECK(merge.contains_point(interval_array4[i].start()));


  // --- merging two intervals that are actually merged
  //   .00  .10  .20  .30  .40  .50
  // 0: |----)
  // 1:             |--)
  // 2                      |----)
  static double starts5[] = { 0.00, 0.25, 0.40, };
  static double ends5[]   = { 0.10, 0.30, 0.50, };

  Interval interval_array5[] = {
    Interval(starts5[0],ends5[0]),
    Interval(starts5[1],ends5[1]),
    Interval(starts5[2],ends5[2]),
  };
  const int no_intervals5 = (sizeof interval_array5)/sizeof(Interval);

  Intervals intervals5;
  for (int i = 0; i < no_intervals5; ++i)
    intervals5.add(interval_array5[i]);

  //   .00  .10  .20  .30  .40  .50
  // 0:      |----)
  // 1:                |----)
  static double starts6[] = {  0.10, 0.30, };
  static double ends6[]   = {  0.20, 0.40, };

  Interval interval_array6[] = {
    Interval(starts6[0],ends6[0], 1),
    Interval(starts6[1],ends6[1], 1),
  };
  const int no_intervals6 = (sizeof interval_array6)/sizeof(Interval);

  Intervals intervals6;
  for (int i = 0; i < no_intervals6; ++i)
    intervals6.add(interval_array6[i]);
 

  // the merge should give us the same intervals as the first interval
  merge = intervals5 | intervals6;
  CHECK(merge.size() == intervals.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));

  merge = intervals6 | intervals5;
  CHECK(merge.size() == intervals.size());
  for (int i = 0; i < no_intervals; ++i)
    CHECK(merge.contains_point(interval_array[i].start()));


  // -- overlaps in the merge -----------------------------------------

  //   .00  .10  .20  .30  .40  .50
  // 0:         |--------)
  static double starts7[] =  {  0.15, };
  static double ends7[] = {  0.32, };

  Interval interval_array7[] = {
    Interval(starts7[0],ends7[0], 1),
  };
  const int no_intervals7 = (sizeof interval_array7)/sizeof(Interval);

  Intervals intervals7;
  for (int i = 0; i < no_intervals7; ++i)
    intervals7.add(interval_array7[i]);

  merge = intervals6 | intervals7;

  // the merge gives us the intervals:
  //   .00  .10  .20  .30  .40  .50
  //         |--)
  //            |-)
  //              |----)
  //                   |-)
  //                     |--)
  

  CHECK(merge.size() == 5);
  CHECK(merge.contains_point(0.10));
  CHECK(merge.contains_point(0.15));
  CHECK(merge.contains_point(0.20));
  CHECK(merge.contains_point(0.30));
  CHECK(merge.contains_point(0.32));

  CHECK(merge[0].leaf_contacts() == 1);
  CHECK(merge[1].leaf_contacts() == 2);	// both cover from intervals 6 and 7
  CHECK(merge[2].leaf_contacts() == 1);
  CHECK(merge[3].leaf_contacts() == 2);	// both cover from intervals 6 and 7
  CHECK(merge[4].leaf_contacts() == 1);

  merge = intervals7 | intervals6;

  CHECK(merge.size() == 5);
  CHECK(merge.contains_point(0.10));
  CHECK(merge.contains_point(0.15));
  CHECK(merge.contains_point(0.20));
  CHECK(merge.contains_point(0.30));
  CHECK(merge.contains_point(0.32));

  CHECK(merge[0].leaf_contacts() == 1);
  CHECK(merge[1].leaf_contacts() == 2);	// both cover from intervals 6 and 7
  CHECK(merge[2].leaf_contacts() == 1);
  CHECK(merge[3].leaf_contacts() == 2);	// both cover from intervals 6 and 7
  CHECK(merge[4].leaf_contacts() == 1);


  CHECK(merge.leaf_contacts(0.00) == 0);
  CHECK(merge.leaf_contacts(0.10) == 1);
  CHECK(merge.leaf_contacts(0.12) == 1);
  CHECK(merge.leaf_contacts(0.15) == 2);
  CHECK(merge.leaf_contacts(0.20) == 1);
  CHECK(merge.leaf_contacts(0.25) == 1);
  CHECK(merge.leaf_contacts(0.30) == 2);
  CHECK(merge.leaf_contacts(0.35) == 1);
  CHECK(merge.leaf_contacts(0.40) == 0);
  CHECK(merge.leaf_contacts(1.00) == 0);


  //   .00  .10  .20  .30  .40  .50
  // 0:         |---)|--)
  static double starts8[] = { 0.15, 0.25, };
  static double ends8[]   = { 0.25, 0.32, };

  Interval interval_array8[] = {
    Interval(starts8[0],ends8[0], 1),
    Interval(starts8[1],ends8[1], 1),
  };
  const int no_intervals8 = (sizeof interval_array8)/sizeof(Interval);

  Intervals intervals8;
  for (int i = 0; i < no_intervals8; ++i)
    intervals8.add(interval_array8[i]);

  merge = intervals7 | intervals8;
  CHECK(merge.size() == 2);
  CHECK(merge.contains_point(0.15));
  CHECK(merge.contains_point(0.25));

  CHECK(merge[0].leaf_contacts() == 2);
  CHECK(merge[1].leaf_contacts() == 2);


  //   .00  .10  .20  .30  .40  .50
  // 0:      |---)     |----)
  static double starts9[] = { 0.10, 0.30, };
  static double ends9[]   = { 0.20, 0.40, };

  //   .00  .10  .20  .30  .40  .50
  // 0:   |--------------------)
  static double starts10[] = { 0.05 };
  static double ends10[]   = { 0.45 };

  Interval interval_array9[] = {
    Interval(starts9[0],ends9[0], 1),
    Interval(starts9[1],ends9[1], 1),
  };
  Interval interval_array10[] = {
    Interval(starts10[0],ends10[0], 2),
  };

  const int no_intervals9  = (sizeof interval_array9)/sizeof(Interval);
  const int no_intervals10 = (sizeof interval_array10)/sizeof(Interval);

  Intervals intervals9;
  for (int i = 0; i < no_intervals9; ++i)
    intervals9.add(interval_array9[i]);
  Intervals intervals10;
  for (int i = 0; i < no_intervals10; ++i)
    intervals10.add(interval_array10[i]);

  merge = intervals9 | intervals10;

  CHECK(merge.size() == 5);
  CHECK(merge.contains_point(0.05));
  CHECK(merge.contains_point(0.10));
  CHECK(merge.contains_point(0.20));
  CHECK(merge.contains_point(0.40));

  CHECK(merge[0].leaf_contacts() == 2);
  CHECK(merge[1].leaf_contacts() == 3);
  CHECK(merge[2].leaf_contacts() == 2);
  CHECK(merge[3].leaf_contacts() == 3);
  CHECK(merge[4].leaf_contacts() == 2);

  merge = intervals10 | intervals9;

  CHECK(merge.size() == 5);
  CHECK(merge.contains_point(0.05));
  CHECK(merge.contains_point(0.10));
  CHECK(merge.contains_point(0.20));
  CHECK(merge.contains_point(0.40));

  CHECK(merge[0].leaf_contacts() == 2);
  CHECK(merge[1].leaf_contacts() == 3);
  CHECK(merge[2].leaf_contacts() == 2);
  CHECK(merge[3].leaf_contacts() == 3);
  CHECK(merge[4].leaf_contacts() == 2);


}



int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  try {

  test_Interval();

  test_Intervals();
  test_Intervals2();
  test_Intervals3();
  test_Intervals4();

  //test_Intervals_add_intervals();
  test_Intervals_sum();
  test_Intervals_merge();

  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }

  REPORT_RESULTS;
}
