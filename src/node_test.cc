
#include "node.hh"
#include "testing.hh"
#include "dist_funcs.hh"

static const double positions[] = { 0.3, 12, 21, 4, 2.2, 41, 33.2, };
static const int no_positions = (sizeof positions)/sizeof(double);


static Intervals *make_intervals()
{
  //    00   10   20   30   40   50
  // 0: |---------|
  // 1:      |----|
  // 2:                |----|
  // 3                      |----|
  static double starts[] =  {  0, 10, 30, 40, };
  static double lengths[] = { 20, 10, 10, 10, };

  Interval* interval_array[] = {
    new Interval(starts[0],lengths[0]),
    new Interval(starts[1],lengths[1]),
    new Interval(starts[2],lengths[2]),
    new Interval(starts[3],lengths[3]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  Intervals *intervals = new Intervals();
  for (int i = 0; i < no_intervals; ++i)
    intervals->add(interval_array[i]);

  return intervals;
}

static void test_Retired_intervals()
{
  try {
    Retired_intervals(0);
    ERROR("Retired_intervals class should not accept a null-pointer\n"
	  "intervals as it will dereference it in the interval() method!\n"
 	  "(that method is poorly named,btw...)");
  } catch(std::exception) {}

  Intervals *intervals = make_intervals();
  Retired_intervals r_intervals(intervals,0); // steals the pointer,
					      // so don't delete
					      // intervals (FIXME?)

  CHECK(&r_intervals.interval() == intervals);
  for (int i = 0; i < intervals->size(); ++i)
    CHECK(&intervals->interval(i) == &r_intervals.interval(i));

  CHECK(r_intervals.connection() == 0);

  const int no_states = no_positions;
  Node n(0,no_states,0,intervals);
  r_intervals.set_connection(&n);
  CHECK(r_intervals.connection() == &n);


  Intervals *intervals2 = make_intervals();
  Retired_intervals r_intervals2(intervals2,&n);
  CHECK(r_intervals2.connection() == &n);

  // FIXME: test write

  // write_leaf_nodes -- just propagates to connection()
  std::string n_nodes, n_haplotypes;
  std::string ri_nodes, ri_haplotypes;

  n.write_leaf_nodes(n_nodes,n_haplotypes);
  n.reset_written(); // -- we call it again in the propagation
  r_intervals2.write_leaf_nodes(ri_nodes,ri_haplotypes);
  CHECK(n_nodes == ri_nodes);
  CHECK(n_haplotypes == ri_haplotypes);

  // reset_written -- just propagates to connection()
  n.reset_written();
  n.write_leaf_nodes(n_nodes,n_haplotypes);
  r_intervals2.reset_written();
  r_intervals2.write_leaf_nodes(ri_nodes,ri_haplotypes);
  CHECK(n_nodes == ri_nodes);
  CHECK(n_haplotypes == ri_haplotypes);

  // count_leaf_snp_states -- propagates to connection() if the
  // interval contains the site

  n.state(0) = 1;

  int site = 0;
  CHECK(r_intervals2.interval().contains_point(Node::position(site)));

  int n_zeros, n_ones;
  int ri_zeros, ri_ones;

  n.count_leaf_snp_states(site, n_zeros,n_ones);
  r_intervals2.count_leaf_snp_states(site, ri_zeros,ri_ones);


  CHECK(n_zeros == ri_zeros);
  CHECK(n_ones == ri_ones);

  ERROR("The counting only works if the counters are initialized\n"
	"to 0 outside the method!");

  n_zeros = n_ones = ri_zeros = ri_ones = 0;

  n.reset_written();
  n.count_leaf_snp_states(site, n_zeros,n_ones);
  n.reset_written();
  r_intervals2.count_leaf_snp_states(site, ri_zeros,ri_ones);

  CHECK(n_zeros == ri_zeros);
  CHECK(n_ones == ri_ones);
  

  ERROR("For some reason snp-counting depends on written???\n");
  int n_zeros2, n_ones2, ri_zeros2, ri_ones2;
  n_zeros2 = n_ones2 = ri_zeros2 = ri_ones2 = 0;

  n.count_leaf_snp_states(site, n_zeros2,n_ones2);
  n.reset_written();
  r_intervals2.count_leaf_snp_states(site, ri_zeros2,ri_ones2);

  CHECK(n_zeros2 == ri_zeros2);
  CHECK(n_ones2 == ri_ones2);


  //FIXME: test evolve
  //FIXME: test write_mutation
  //FIXME: test set_is_mutating_on_subtree
  //FIXME: test evolve_snp
}

static void test_Node()
{
  try {
    Node(0,0,0,0);
    ERROR("Node class should not accept a null-pointer intervals\n"
	  "as it will dereference it in the interval() method!\n"
 	  "(that method is poorly named,btw...)");
  } catch(std::exception) {}

  ERROR("Explicitly specifying the node type in the Node constructor\n"
	"is problematic as there is no validation of the correctness of\n"
	"the type value (nor is it a particularly OO design).");

  ERROR("Explicitly specifying the number of sites is a problem as well\n"
	"as the only size that will validate is the global number of\n"
	"positions.");
  try {
    Intervals dummy;
    Node(0,0,0,&dummy);
    ERROR("The Node class should not accept an invalide number of positions!");
  } catch(std::exception) {}

  const double time = 12.2;
  const unsigned int no_states = no_positions;
  Intervals empty_intervals;
  Node n1(time,no_states,0,&empty_intervals);

  CHECK(n1.validate());

  CHECK(!n1.has_intervals());
  CHECK(n1.node_type() == 0);
  CHECK(n1.size() == no_states);
  CHECK(n1.time() == time);

  n1.set_time(time+2);
  CHECK(n1.time() == time+2);

  for (int i = 0; i < no_positions; ++i)
    CHECK(!n1.is_mutating(i));

  n1.set_is_mutating(true,2);
  n1.set_is_mutating(true,4);

  CHECK(!n1.is_mutating(0));
  CHECK(!n1.is_mutating(1));
  CHECK(n1.is_mutating(2));
  CHECK(!n1.is_mutating(3));
  CHECK(n1.is_mutating(4));
  CHECK(!n1.is_mutating(5));

  for (int i = 0; i < no_positions; ++i)
    n1.set_is_mutating(true,i);
  for (int i = 0; i < no_positions; ++i)
    CHECK(n1.is_mutating(i));

  try {
    //n1.is_mutating(-1); -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}
  try {
    //n1.set_is_mutating(true,-1); -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}

  try {
    //n1.is_mutating(no_positions);  -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}
  try {
    //n1.set_is_mutating(true,no_positions);  -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}


  for (int i = 0; i < no_positions; ++i)
    CHECK(n1.state(i) == -1);	// all initialized as missing data
  for (int i = 0; i < no_positions; ++i)
    CHECK(n1[i] == -1);		// all initialized as missing data

  ERROR("There is no consistency check when setting the state!");
  for (int i = 0; i < no_positions; ++i)       n1[i] =  i;
  for (int i = 0; i < no_positions; ++i) CHECK(n1[i] == i);

  try {
    //n1[-1]; -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}
  try {
    //n1[-1] = 1; -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}

  try {
    //n1[no_positions];  -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}
  try {
    //n1[no_positions] = 1;  -- currently segfaults
    ERROR("Indexing outside of the existing range should not be allowed!");
  } catch(std::exception) {}



  ERROR("I am not sure this is the expected behaviour! (I didn't expect it)");
  CHECK(n1.active_left() == 1.0);
  CHECK(n1.active_right() == 0.0);



  Intervals *intervals = make_intervals();
  Node n2(time,no_states,0,intervals);

  CHECK(n2.validate());

  CHECK(n2.has_intervals());
  CHECK(&n2.interval() == intervals);
  for (int i = 0; i < intervals->size(); ++i)
    CHECK(&n2.interval(i) == &intervals->interval(i));

  ERROR("This only works on sorted intervals, but the Intervals class\n"
	"does not guarantee that the intervals are sorted.");
  CHECK(n2.active_left() == 0.0);
  CHECK(n2.active_right() == 50.0);

  CHECK(!n2.is_active_region(-1,-.5));
  CHECK(!n2.is_active_region(-.5,-.5));

  CHECK(n2.is_active_region(-.5,.5));

  CHECK(n2.is_active_region( 0, 0));
  CHECK(n2.is_active_region( 0,10));
  CHECK(n2.is_active_region( 0,20));
  CHECK(n2.is_active_region(10,20));
  CHECK(n2.is_active_region( 5,15));
  CHECK(n2.is_active_region(10,10));
  CHECK(n2.is_active_region(20,20));

  CHECK(!n2.is_active_region(21,29));
  CHECK(!n2.is_active_region(25,25));

  CHECK(n2.is_active_region(30,30));
  CHECK(n2.is_active_region(30,31));
  CHECK(n2.is_active_region(31,32));

  CHECK(n2.is_active_region(35,45));

  CHECK(n2.is_active_region(40,50));
  CHECK(n2.is_active_region(50,50));

  CHECK(n2.is_active_region(49,51));

  CHECK(!n2.is_active_region(51,51));
  CHECK(!n2.is_active_region(52,60));

  ERROR("This was not the behaviour I expected from a function with the name\n"
	"contains_active_region()!");
  CHECK(n2.contains_active_region(31,35));

  // disable mutation
  Distribution_functions::uniform_result = 1.0;
  Distribution_functions::expdist_result = 0.0;
  CHECK(n2.mutate(0,0,0) == n2.state(0));

  // force mutation
  Distribution_functions::uniform_result = 0.0;
  Distribution_functions::expdist_result = 1.0;
  CHECK(n2.mutate(0,0,0) != n2.state(0));

  // dataset at marker 0 is 0,1,3
  Distribution_functions::irand_result = 0;
  CHECK(n2.mutate(0,0,0) == 0); CHECK(n2[0] == 0);
  Distribution_functions::irand_result = 1;
  CHECK(n2.mutate(0,0,0) == 1); CHECK(n2[0] == 1);
  Distribution_functions::irand_result = 2;
  CHECK(n2.mutate(0,0,0) == 3); CHECK(n2[0] == 3);

  ERROR("Is this the intended behaviour?  Usually a mutation changes the\n"
	"state, but apparently not when it is the last value");
  CHECK(n2.mutate(0,0,0) == 3);

  ERROR("For mutate_snp, force *enables* but does not force the mutation!");
  n2[1] = 0;
  Distribution_functions::irand_result = 0;

  // it appears that, mutate_snp does not change the state, in
  // contrast to mutate
  CHECK(n2.mutate_snp(1,true) == 1);
  CHECK(n2.mutate_snp(1,true) == 0);


  n2[1] = 0;
  int zeros, ones;
  n2.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 1); CHECK(ones == 0);

  n2[1] = 1;
  n2.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 0); CHECK(ones == 1);

  ERROR("Counting only works if we manually initialize the counters\n"
	"AND reset the written flag.");
  n2[1] = 0; n2.reset_written();
  zeros = ones = 0;
  n2.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 1); CHECK(ones == 0);

  n2[1] = 1; n2.reset_written();
  zeros = ones = 0;
  n2.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 0); CHECK(ones == 1);
  
  

  // FIXME: these methods have not been tested
  /*
      Coalescent_node* operator+(Node& n);
  void genconversion(Genconversion_node*& gcon_node_1, Genconversion_node*& gcon_node_2, double Q, double time);
  void recombination(Recombination_node*& rcom_node_1, Recombination_node*& rcom_node_2, double time);


  std::string write_haplotype();
 
  void set_written(bool v = false){ written = v; };
  virtual void reset_written();

  void reset_node(double time, bool leaf_intervals = false, Intervals* i_val = 0)

  virtual void set_is_mutating_on_subtree(bool m, int site, double depth, double& cur_depth){};
  virtual void collect_sub_tree_surface(int site, double& surface){};
  virtual void evolve(int site, double mu){};
  virtual void evolve_snp(int site){};
  virtual void write(std::string& _nodes, std::string& _haplotypes);
  virtual void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  virtual void write_mutation(std::string& mutation){};

  */

  delete intervals;
}

static Intervals *make_left_intervals()
{
  //    00   10   20   30   40   50
  // 0: |---------|
  // 1                      |----|
  static double starts[] =  {  0, 40, };
  static double lengths[] = { 20, 10, };

  Interval* interval_array[] = {
    new Interval(starts[0],lengths[0]),
    new Interval(starts[1],lengths[1]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  Intervals *intervals = new Intervals();
  for (int i = 0; i < no_intervals; ++i)
    intervals->add(interval_array[i]);

  return intervals;
}

static Intervals *make_right_intervals()
{
  //    00   10   20   30   40   50
  // 0:                |----|
  // 1:      |----|
  static double starts[] =  { 30, 10, };
  static double lengths[] = { 10, 10, };

  Interval* interval_array[] = {
    new Interval(starts[0],lengths[0]),
    new Interval(starts[1],lengths[1]),
  };
  const int no_intervals = (sizeof interval_array)/sizeof(Interval*);

  Intervals *intervals = new Intervals();
  for (int i = 0; i < no_intervals; ++i)
    intervals->add(interval_array[i]);

  return intervals;
}


static void test_Coalescent_node()
{
  try {
    Coalescent_node::get_new_coalescent_node(0,0,0,0);
    ERROR("Coalescent_node::get_new_coalescent_node should not accept NULL\n"
	  "children as it dereferences them without testing for NULL");
  } catch(std::exception) {};

  Intervals *l_intervals = make_left_intervals();
  Intervals *r_intervals = make_right_intervals();

  Node left(0.0, no_positions, 0, l_intervals);
  Node right(0.0, no_positions, 0, r_intervals);

  try {
    Coalescent_node::get_new_coalescent_node(&left,&right,0,0);
    ERROR("Coalescent_node::get_new_coalescent_node should not accept NULL\n"
	  "as the interval as it dereferences it without testing for NULL");
  } catch(std::exception) {};

  Intervals intervals;
  Coalescent_node &cnode =
    Coalescent_node::get_new_coalescent_node(&left,&right,0,&intervals);

  CHECK(cnode.left_child() == &left);
  CHECK(cnode.right_child() == &right);

  try {
    cnode.set_left_child(0);
    ERROR("Coalescent_node should not permit a child to be set to 0.");
  } catch(std::exception) {}
  try {
    cnode.set_right_child(0);
    ERROR("Coalescent_node should not permit a child to be set to 0.");
  } catch(std::exception) {}

  Node left2(0.0, no_positions, 0, l_intervals);
  Node right2(0.0, no_positions, 0, r_intervals);

  cnode.set_left_child(&left2);
  cnode.set_right_child(&right2);
  CHECK(cnode.left_child() == &left2);
  CHECK(cnode.right_child() == &right2);

  CHECK(&cnode.left_interval() == &left2.interval());
  CHECK(&cnode.right_interval() == &right2.interval());

  CHECK(!cnode.is_left_interval(-1));
  CHECK( cnode.is_left_interval( 0));
  CHECK( cnode.is_left_interval(10));
  CHECK( cnode.is_left_interval(20));
  CHECK(!cnode.is_left_interval(25));
  CHECK(!cnode.is_left_interval(30));
  CHECK( cnode.is_left_interval(40));
  CHECK( cnode.is_left_interval(45));
  CHECK(!cnode.is_left_interval(55));

  CHECK(!cnode.is_right_interval(-1));
  CHECK(!cnode.is_right_interval( 0));
  CHECK( cnode.is_right_interval(10));
  CHECK( cnode.is_right_interval(15));
  CHECK( cnode.is_right_interval(20));
  CHECK(!cnode.is_right_interval(25));
  CHECK( cnode.is_right_interval(30));
  CHECK( cnode.is_right_interval(40));
  CHECK(!cnode.is_right_interval(45));

  // FIXME: I'm not sure this is the intended behaviour, but I think
  // it should be...?
  CHECK(!cnode.interval().contains_point(-1));
  CHECK( cnode.interval().contains_point( 0));
  CHECK( cnode.interval().contains_point(10));
  CHECK( cnode.interval().contains_point(15));
  CHECK( cnode.interval().contains_point(20));
  CHECK(!cnode.interval().contains_point(25));
  CHECK( cnode.interval().contains_point(30));
  CHECK( cnode.interval().contains_point(35));
  CHECK( cnode.interval().contains_point(40));
  CHECK( cnode.interval().contains_point(45));
  CHECK( cnode.interval().contains_point(50));
  CHECK(!cnode.interval().contains_point(55));


  int zeros, ones;
  left2[1] = 0; right2[1] = 0;
  cnode.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 2); CHECK(ones == 0);

  left2[1] = 1; right2[1] = 0;
  cnode.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 1); CHECK(ones == 1);

  left2[1] = 0; right2[1] = 1;
  cnode.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 1); CHECK(ones == 1);

  left2[1] = 1; right2[1] = 1;
  cnode.count_leaf_snp_states(1,zeros,ones);
  CHECK(zeros == 0); CHECK(ones == 2);


  // force mutation
  Distribution_functions::uniform_result = 0.0;
  Distribution_functions::expdist_result = 1.0;

  // dataset at marker 0 is 0,1,3
  left2[0] = 1;
  Distribution_functions::irand_result = 0;
  CHECK(cnode.left_mutate(0,0) == 0);
  CHECK(left2[0] == 0);
  CHECK(cnode.left_is_mutating(0));

  cnode.set_left_is_mutating(0,false);
  CHECK(!cnode.left_is_mutating(0));

  Distribution_functions::irand_result = 1;
  CHECK(cnode.left_mutate(0,0) == 1);
  CHECK(left2[0] == 1);
  CHECK(cnode.left_is_mutating(0));

  cnode.set_left_is_mutating(0,false);
  CHECK(!cnode.left_is_mutating(0));

  Distribution_functions::irand_result = 3;
  CHECK(cnode.left_mutate(0,0) == 3);
  CHECK(left2[0] == 3);
  CHECK(cnode.left_is_mutating(0));

  right2[0] = 1;
  Distribution_functions::irand_result = 0;
  CHECK(cnode.right_mutate(0,0) == 0);
  CHECK(right2[0] == 0);
  CHECK(cnode.right_is_mutating(0));

  cnode.set_right_is_mutating(0,false);
  CHECK(!cnode.right_is_mutating(0));

  Distribution_functions::irand_result = 1;
  CHECK(cnode.right_mutate(0,0) == 1);
  CHECK(right2[0] == 1);
  CHECK(cnode.right_is_mutating(0));

  cnode.set_right_is_mutating(0,false);
  CHECK(!cnode.right_is_mutating(0));

  Distribution_functions::irand_result = 3;
  CHECK(cnode.right_mutate(0,0) == 3);
  CHECK(right2[0] == 3);
  CHECK(cnode.right_is_mutating(0));

  

  //FIXME: these methods are not tested...
  /*
  bool retire_finished_intervals(int leaf_nodes_size);
  Intervals* has_finished_intervals(int leaf_nodes_size);
  void set_is_mutating_on_subtree(bool m,  int site, double depth, double& cur_depth);
  void collect_sub_tree_surface(int site, double& surface);

  void reset_written();
  void write_mutation(std::string& mutation);
  void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  void write(std::string& _nodes, std::string& _haplotypes);
  void evolve_snp(int site);
  void evolve(int site, double mu);

  */
}

static void test_Recombination_node()
{
  // FIXME
}

static void test_Genconversion_node()
{
  // FIXME
}


// some global setup ...
static void setup_node_positions()
{
  for (int i = 0; i < no_positions; ++i)
    Node::add_position(positions[i]);

  CHECK(Node::position_size() == no_positions);

  for (int i = 0; i < no_positions; ++i)
    CHECK(Node::position(i) == positions[i]);

  ERROR("There is *way* too much manual initialisation here,\n"
	"and the code does *not* handle access to uninitialised\n"
	"node sets gracefully -- it will most likely return garbage\n"
	"or crash!");

  Node::init_mutation_type();
  ERROR("Avoid the magical constants here, use enum!");
  Node::set_mutation_type(0,0);
  CHECK(Node::is_microsattelite(0));

  Node::set_mutation_type(1,1);
  CHECK(Node::is_snp(1));

  Node::set_mutation_type(2,2);
  CHECK(Node::is_disease_marker(2));


  Node::init_value_set();
  Node::add_to_set_value(0,0);
  Node::add_to_set_value(0,1);
  Node::add_to_set_value(0,3);

  Node::add_to_set_value(1,0);
  Node::add_to_set_value(1,1);
  try {
    Node::add_to_set_value(1,3);
    ERROR("A SNP marker should only contain 0/1 values!");
  } catch(std::exception) {}

  // FIXME: test value_set, value_set_value

  Node::init_freq();

  // FIXME: test set_low_freq/low_freq and set_high_freq/high_freq

}

static void setup()
{
  setup_node_positions();
}

int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  setup();

  test_Retired_intervals();
  test_Node();
  test_Coalescent_node();
  test_Recombination_node();
  test_Genconversion_node();    

  REPORT_RESULTS;
}
