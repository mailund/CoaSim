#ifndef NODE_HH
#define NODE_HH

#include "interval.hh"
#include "configuration.hh"

#include <stdexcept>
#include <algorithm>
#include <vector>

class ARG
{
public:

  /* Abstract class for ARG nodes. */
  class Node 
  {
  public:
    Node(double time) : _time(time) {}
    Node(double time, const Intervals &i) : _time(time), _intervals(i) {}
    virtual ~Node() {};

    const Intervals &intervals() const { return _intervals; }

  private:
    double    _time;
    Intervals _intervals;
  };

  // Exception thrown if a node is created with a 0-child
  class null_child : public std::exception {};


  // Initialization and book-keeping
  ARG(const Configuration &conf) : _conf(conf) {}

  // Cleanup.  Destroying the ARG also deletes all nodes in it, so
  // don't keep any pointers to them around after this deletion.
  ~ARG();


  // Factory methods for building the ARG
  Node *leaf()                                            throw();
  Node *coalescence(double time, Node *left, Node *right) throw(null_child);
  // these methods return a pair of new nodes, if two nodes were
  // actually created, or the child node (as the first element in
  // pair, the second being 0), if one of the nodes created would
  // otherwise be immediately retired
  typedef std::pair<Node*,Node*> node_pair_t;
  node_pair_t recombination(double time, Node *child)     throw(null_child);
  node_pair_t gene_conversion(double time, Node *child)   throw(null_child);


private:
  const Configuration &_conf;  

  // pools of nodes -- FIXME: can be handled more efficiently...
  std::vector<Node*> _node_pool;
};


#if 0
class Coalescent_node;
class Genconversion_node;
class Recombination_node;
class Node;

class Retired_intervals
{
public:
  Retired_intervals( Intervals* i_val, Node* child = 0) : _interval(i_val), _child(child) { };
 ~Retired_intervals(){ delete _interval; };
  Intervals& interval(){ return *_interval; };
  const Interval& interval(int index){ return _interval->interval(index); };
  Node* connection(){ return _child; };
  void set_connection(Node* n){ _child = n; };
  void write(std::string& _nodes, std::string& _haplotypes);
  void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  void evolve(int site, double mu);
  void reset_written();
  void write_mutation(std::string& mutation);
  void set_is_mutating_on_subtree(bool forced, int site, double& surface);
  void evolve_snp(int site);
  void count_leaf_snp_states(int site, int& zeros, int& ones);
private:
  Intervals* _interval;
  Node* _child;
};

class Node 
{
public:
  Node(double time, int s, int node_type, Intervals* i_val) : _intervals(i_val), _node_type(node_type), 
							     _time(time), _states(-1,s), written(false),
							     _is_mutating(false,s) { };
  // time is the time of the node, s is the number og markers on the node, node_type indicates if the node is a coalescent node (0), 
  // recombinatin node (2), geneconversion node (1) and leafnode (-1)
  // i_val is the intervals (remember the s for pural) that is associated with the node
  virtual ~Node(){ std::cout << "Node destructor called" << std::endl; };

  bool has_intervals(){ return (interval().size()!=0); };
  int node_type(){return _node_type; };
  unsigned int size() {return _states.size(); };

  void set_time(double d){ _time = d;}; 
  double time(){ return  _time; };

  bool is_mutating(int site){ return _is_mutating[site]; };
  void set_is_mutating(bool m, int site){ _is_mutating[site] = m; };

  Coalescent_node* operator+(Node& n);
  void genconversion(Genconversion_node*& gcon_node_1, Genconversion_node*& gcon_node_2, double Q, double time);
  void recombination(Recombination_node*& rcom_node_1, Recombination_node*& rcom_node_2, double time);


  bool contains_active_region(double left, double right){ return ((left<=active_right())&&(right>=active_left())); };
  bool is_active_region(double left, double right); 
  bool validate(){ return (Node::_pos.size()==size()); };
  double active_left(){ return (interval().size()) ? interval(0).start() : 1.0; };
  double active_right(){ return (interval().size()) ? interval(interval().size()-1).end() : 0.0; };
  int& state(int index){return _states[index]; };
  int& operator[](int index) { return _states[index]; };
  std::string write_haplotype();
  int mutate(const double mu, const double time, int site);
  int mutate_snp(int site, bool forced);
 
  Intervals& interval(){ return *_intervals; };
  const Interval& interval(int index){ return _intervals->interval(index); };
  //  void set_intervals(Intervals intervals){ _intervals=&intervals; };
  void set_written(bool v = false){ written = v; };
  void reset_node(double time, bool leaf_intervals = false, Intervals* i_val = 0)
  {
    set_time(time);
    if (!leaf_intervals) {
      delete _intervals;
      _intervals = i_val;
    };
    for (unsigned int i=0; i<size(); i++){
      set_is_mutating(false,  i);
      state(i) = -1;
    }
    set_written(); 
  };

  virtual void reset_written();
  virtual void count_leaf_snp_states(int site, int& zeros, int& ones);
  virtual void set_is_mutating_on_subtree(bool m, int site, double depth, double& cur_depth){};
  virtual void collect_sub_tree_surface(int site, double& surface){};
  virtual void evolve(int site, double mu){};
  virtual void evolve_snp(int site){};
  virtual void write(std::string& _nodes, std::string& _haplotypes);
  virtual void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  virtual void write_mutation(std::string& mutation){};


  static double position(int i){ return _pos[i];};
  static int position_size(){ return _pos.size();};
  static void add_position(double d){ _pos.push_back(d);};
  static std::vector<int>& value_set(int marker){ return _value_set[marker]; };
  static int value_set_value(int marker, int index){ return _value_set[marker][index]; };
  static void add_to_set_value(int marker, int value) { _value_set[marker].push_back(value); };
  static void init_value_set()
  { 
    _value_set.resize(_pos.size());
    for (unsigned int i=0; i< _value_set.size(); i++){
      _value_set[i].resize(0);
    }
  }
  static void set_mutation_type(int index, int marker_type){ _mutation_type[index] = marker_type; };
  static void init_mutation_type(){
    _mutation_type.resize(0,_pos.size());
  }
  static bool is_snp(int index){ return (_mutation_type[index]==1); }; // 0 for microsatelites and 1 for snp and 2 for a disease marker
  static bool is_microsattelite(int index){ return (_mutation_type[index]==0); }; // 0 for microsatelites and 1 for snp and 2 for a disease marker
  static bool is_disease_marker(int index){ return (_mutation_type[index]==2); }; // 0 for microsatelites and 1 for snp and 2 for a disease marker

  static void init_freq(){
    _low_freq.resize(_pos.size());
    _high_freq.resize(_pos.size());
  }
  static void set_low_freq(int index, double f){ _low_freq[index] = f; };
  static double low_freq(int index){ return _low_freq[index]; };
  static void set_high_freq(int index, double f){ _high_freq[index] = f; };
  static double high_freq(int index){ return _high_freq[index]; };
protected:
  Intervals* _intervals;
  int _node_type;
  double _time;
  std::valarray<int> _states;
  bool written;
  std::valarray<bool> _is_mutating;
  static std::valarray<int> _mutation_type;
  static std::vector< double > _pos;
  static std::vector< double > _low_freq;
  static std::vector< double > _high_freq;
  static std::vector< std::vector<int> > _value_set;
};


class Coalescent_node : public Node 
{
public:
  ~Coalescent_node(){std::cout << "Coalescent destructor called" << std::endl;};
  static Coalescent_node& get_new_coalescent_node(Node* left_child, Node* right_child, int size, Intervals* i_val)
  {
    if (next_node == int(coalescent_nodes.size()-1)){
      coalescent_nodes.push_back(new Coalescent_node(left_child,right_child, size, i_val));
    }
    else{
      
      coalescent_nodes[next_node+1] -> set_left_child(left_child);
      coalescent_nodes[next_node+1] -> set_right_child(right_child);
      for (int i=0; i<size; i++){
	coalescent_nodes[next_node+1] -> set_left_is_mutating(false,i);
	coalescent_nodes[next_node+1] -> set_right_is_mutating(false,i);
      }
      coalescent_nodes[next_node+1] -> reset_node(0.0,false,i_val);
    }
    next_node++;
    return *(coalescent_nodes[next_node]);
  }
  static void cleanup(){ next_node = -1; };

  bool retire_finished_intervals(int leaf_nodes_size);
  Intervals* has_finished_intervals(int leaf_nodes_size);
  void set_is_mutating_on_subtree(bool m,  int site, double depth, double& cur_depth);
  void collect_sub_tree_surface(int site, double& surface);
  bool is_left_interval(double pos);
  bool is_right_interval(double pos);
  void reset_written();
  void count_leaf_snp_states(int site, int& zeros, int& ones);
  void set_left_is_mutating(bool m, int site){ _left_is_mutating[site] = m; };
  void set_right_is_mutating(bool m, int site){ _right_is_mutating[site] = m; };
  bool left_is_mutating(int site){ return _left_is_mutating[site]; };
  bool right_is_mutating(int site){ return _right_is_mutating[site]; };
  void write_mutation(std::string& mutation);
  void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  void write(std::string& _nodes, std::string& _haplotypes);
  void evolve_snp(int site);
  void evolve(int site, double mu);
  int left_mutate(const double mu, int site);
  int right_mutate(const double mu, int site);
  Node* left_child(){ return _left_child;}
  void set_left_child(Node* c){ _left_child = c; }; 
  Node* right_child(){ return _right_child;}
  void set_right_child(Node* c){ _right_child = c; }; 
  Intervals& left_interval(){ return left_child()->interval(); };
  const Interval& left_interval(int index){ return left_child()->interval().interval(index); };
  Intervals& right_interval(){ return right_child()->interval(); };
  const Interval& right_interval(int index){ return right_child()->interval().interval(index); };
private:
  Coalescent_node(Node* left_child, Node* right_child, int size, Intervals* i_val);
  Node* _left_child;
  Node* _right_child;
  std::valarray<bool> _left_is_mutating;
  std::valarray<bool> _right_is_mutating;
  static std::vector<Coalescent_node*> coalescent_nodes;
  static int next_node;
};

class Recombination_node : public Node 
{
public:
  ~Recombination_node(){std::cout << "Recombination destructor called" << std::endl;};
  static Recombination_node& get_new_recombination_node(bool is_l, Node* child, double cross_over, double time, int size, Intervals* i_val)
  {
    if (next_node == int(recombination_nodes.size()-1)){
      recombination_nodes.push_back(new Recombination_node(is_l,child, cross_over, time, size, i_val));
    }
    else {
      recombination_nodes[next_node+1] -> set_is_left(is_l);
      recombination_nodes[next_node+1] -> set_child(child);
      recombination_nodes[next_node+1] -> set_cross_over(cross_over);
      recombination_nodes[next_node+1] -> reset_node(time,false,i_val);
    }
    next_node++;
    return *(recombination_nodes[next_node]);
  };
  static void cleanup(){ next_node = -1; };
  void reset_written();
  void count_leaf_snp_states(int site, int& zeros, int& ones);
  void set_is_mutating_on_subtree(bool m,  int site, double depth, double& cur_depth);
  void collect_sub_tree_surface(int site, double& surface);
  void evolve_snp(int site);
  void evolve(int site, double mu);
  void write(std::string& _nodes, std::string& _haplotypes);
  void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  void write_mutation(std::string& mutation);
  double cross_over(){ return _cross_over_point; };
  void set_cross_over(double d){ _cross_over_point = d; };
  Node* child(){ return _child;}
  void set_child(Node* c){ _child = c; }; 
  bool is_left(){return _is_left; };
  void set_is_left(bool l = true){ _is_left = l; };
private:
  Recombination_node(bool is_l, Node* child, double cross_over, double time, int size, Intervals* i_val);
  bool _is_left;
  Node* _child;
  double _cross_over_point;
  static std::vector<Recombination_node*> recombination_nodes;
  static int next_node;
};

class Genconversion_node : public Node 
{
public:
  ~Genconversion_node(){std::cout << "Geneconversion destructor called" << std::endl;};

  static Genconversion_node& get_new_geneconversion_node(bool is_i, Node* child, double conversion_point, 
		     double conversion_length, double time, int size, Intervals* i_val)
  {
    if (next_node == int(geneconversion_nodes.size()-1)){
      geneconversion_nodes.push_back(new Genconversion_node(is_i, child, conversion_point, conversion_length, time, size, i_val)) ;
    }
    else {
      geneconversion_nodes[next_node+1] -> set_is_inside(is_i);
      geneconversion_nodes[next_node+1] -> set_child(child);
      geneconversion_nodes[next_node+1] -> set_conversion_point(conversion_point);
      geneconversion_nodes[next_node+1] -> set_conversion_length(conversion_length);
      geneconversion_nodes[next_node+1] -> reset_node(time,false,i_val);
    }
    next_node++;
    return *(geneconversion_nodes[next_node]);
  };
  static void cleanup(){ next_node = -1; };


  void set_is_mutating_on_subtree(bool m,  int site, double depth, double& cur_depth);
  void collect_sub_tree_surface(int site, double& surface);
  void evolve_snp(int site);
  void evolve(int site, double mu);
  void reset_written();
  void count_leaf_snp_states(int site, int& zeros, int& ones);
  void write(std::string& _nodes, std::string& _haplotypes);
  void write_leaf_nodes(std::string& _nodes, std::string& _haplotypes);
  void write_mutation(std::string& mutation);
  double conversion_point(){ return _conversion_point; };
  void set_conversion_point(double d){ _conversion_point = d; };
  double conversion_length(){ return _conversion_length; };
  void set_conversion_length(double d){ _conversion_length = d; };
  double conversion_start();
  double conversion_end();
  Node* child(){ return _child;}
  void set_child(Node* c){ _child = c; }; 
  bool is_inside(){return _is_inside; };
  void set_is_inside(bool v = false) { _is_inside = v; };
private:
  Genconversion_node(bool is_i, Node* child, double conversion_point, 
		     double conversion_length, double time, int size, Intervals* i_val); 
  bool _is_inside;
  double _conversion_point;
  double _conversion_length;
  Node* _child;
  static std::vector<Genconversion_node*> geneconversion_nodes;
  static int next_node;
};
#endif // 0


#endif
