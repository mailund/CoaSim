
#include "node.hh"

#ifndef COMPILE_OPTIONS_HH
# include "compile_options.hh"
#endif

#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif
#ifndef MARKER_HH_INCLUDED
# include "marker.hh"
#endif

#ifndef SSTREAM_INCLUDED
# include <sstream>
# define SSTREAM_INCLUDED
#endif
#ifndef STRING_INCLUDED
# include <string>
# define STRING_INCLUDED
#endif

#ifndef CASSERT_INCLUDED
# include <cassert>
# define CASSERT_INCLUDED
#endif
#if EXPENSIVE_ASSERTS
# ifndef FSTREAM_INCLUDED
#  include <fstream>
#  define FSTREAM_INCLUDED
# endif
#endif

#ifndef CSTDLIB_INCLUDED
# include <cstdlib>
# define CSTDLIB_INCLUDED
#endif

void Node::haplotype_to_xml(std::ostream &os) const
{
  os << "    <haplotype id=\"h_" << this << "\"> " << std::endl;
  for (unsigned int i = 0; i < i_states.size(); ++i){
    os << "      <loci marker_ref=\"marker_" << i << "\">";
    os << "<allele>" << i_states[i] << "</allele>";
    os << "</loci>" << std::endl;
  }
  os << "    </haplotype>" << std::endl;
}

void Node::initialize_marker(unsigned int idx, const Marker &m)
{
  if (i_states.size() <= idx) 
    throw std::out_of_range("marker index out of range");
  i_states[idx] = m.default_value();
}


namespace {

  class LeafNode : public Node
  {
    friend Node *ARG::leaf();
    LeafNode(const Configuration &conf) : Node(conf,0.0) {}

    virtual double surface_at_point(double point) const
      throw(std::out_of_range)
    {
      if (point < 0 or 1.0 <= point) 
	throw std::out_of_range("Point out of range [0,1).");
      return 0.0;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      // no mutations out of leaf
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <leaf time=\"" << time() << "\" id=\"i_" << this << '"'
	 << " haplotype=\"h_" << this << "\"/>" << std::endl;
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      // nope -- you cannot mutate out of a leaf
    }
  };


  // WARNING: None of the following classes checks whether they are
  // initialized with a null-child, but will crash if that is the
  // case.  They should only be created with the corresponding factory
  // method anyway, and it checks for it, so *DON'T* create these
  // objects in any other way!

  class CoalescentNode : public Node
  {
    friend Node *ARG::coalescence(double,Node*,Node*);
    CoalescentNode(const Configuration &conf, double time, 
		   Node *left, Node *right, const Intervals &is)
      : Node(conf,time,is), i_left(left), i_right(right),
	i_left_mutating(false,conf.no_markers()),
	i_right_mutating(false,conf.no_markers()),
	i_conf(conf)
    {}

    virtual double surface_at_point(double point) const
      throw(std::out_of_range)
    {
      // NB! don't check if this node contains it -- it could be
      // retired, if that is the case it's children will contain it.
      // if this node does not contain it, neither of it's children
      // will, so it still works out (althoug we call recursively at
      // little more than strictly necessary)
      double surface = 0.0;
      if (i_left->intervals().contains_point(point))
	{
	  surface += i_left->surface_at_point(point);
	  surface += time() - i_left->time();
	}
      if (i_right->intervals().contains_point(point))
	{
	  surface += i_right->surface_at_point(point);
	  surface += time() - i_right->time();
	}
      return surface;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      if (! (idx < no_states()) )
	throw std::out_of_range("marker index out of range");

      double point = i_conf.position(idx);

      if (i_left->intervals().contains_point(point))
	{
	  // propagate value
	  set_state(i_left,idx,state(idx));
	  if (m.edge_has_mutation(time(),i_left->time()))
	    {
	      set_state(i_left, idx, m.mutate_to(*i_left,idx));
	      i_left_mutating[idx] = true;
	    }
	  i_left->mutate_marker(idx,m);
	}


      if (i_right->intervals().contains_point(point))
	{
	  set_state(i_right,idx,state(idx));
	  if (m.edge_has_mutation(time(),i_right->time()))
	    {
	      set_state(i_right, idx, m.mutate_to(*i_right,idx));
	      i_right_mutating[idx] = true;
	    }
	  i_right->mutate_marker(idx,m);
	}
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <coalescent time=\"" << time() << "\" id=\"i_" << this << '"'
	 << " haplotype=\"h_" << this << "\">" << std::endl;
      os << "    <child ref=\"i_" << i_left << "\"/>" << std::endl;
      os << "    <child ref=\"i_" << i_right << "\"/>" << std::endl;
      os << "  </coalescent>" << std::endl; 
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      for (size_t i = 0; i < no_states(); ++i){
	if (i_left_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << i_left << "\"/> "
	     << std::endl;
	else if (i_right_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << i_right << "\"/> "
	     << std::endl;
      }
    }

    Node *const i_left;
    Node *const i_right;
    std::valarray<bool> i_left_mutating;
    std::valarray<bool> i_right_mutating;
    const Configuration &i_conf;
  };
  
  class RecombinationNode : public Node
  {
    friend ARG::node_pair_t ARG::recombination(double,Node*,double);
    RecombinationNode(const Configuration &conf,
		      double time, Node *child, const Intervals &is,
		      double cross_over_point, bool is_left)
      : Node(conf,time,is), i_child(child),
	i_child_mutating(false,conf.no_markers()),
	i_cross_over_point(cross_over_point), i_is_left(is_left)
    {}

    virtual double surface_at_point(double point) const
      throw(std::out_of_range)
    {
      if (! intervals().contains_point(point)) return 0.0;

      double surface = 0.0;
      if (i_child->intervals().contains_point(point))
	{
	  surface += i_child->surface_at_point(point);
	  surface += time() - i_child->time();
	}
      return surface;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      if (! (idx < no_states()) )
	throw std::out_of_range("marker index out of range");

      // propagate value
      set_state(i_child,idx,state(idx));

      if (m.edge_has_mutation(time(),i_child->time()))
	{
	  set_state(i_child,idx,m.mutate_to(*i_child,idx));
	  i_child_mutating[idx] = true;
	}
      i_child->mutate_marker(idx,m);
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <recombination time=\"" << time() << '"'
	 << " crossover=\"" << i_cross_over_point << '"'
	 << " id=\"i_" << this << "\" haplotype=\"h_" << this << '"'
	 << " is_left=\"" << i_is_left << "\">" << std::endl
	 << "    <child ref=\"i_" << i_child << "\"/>" << std::endl
	 << "  </recombination>" << std::endl; 
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      for (size_t i = 0; i < no_states(); ++i)
	if (i_child_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << i_child << "\"/> " << std::endl; 
    }

    Node *const i_child;
    std::valarray<bool> i_child_mutating;
    double i_cross_over_point;
    bool i_is_left;
  };

  class GeneConversionNode : public Node
  {
    friend ARG::node_pair_t ARG::gene_conversion(double,Node*,double,double);
    GeneConversionNode(const Configuration &conf,
		       double time, Node *child, const Intervals &is,
		       double conversion_start, double conversion_end,
		       bool is_inside)
      : Node(conf,time,is), i_child(child),
	i_child_mutating(false,conf.no_markers()),
	i_conversion_start(conversion_start), i_conversion_end(conversion_end),
	i_is_inside(is_inside)
    {}

    virtual double surface_at_point(double point) const
      throw(std::out_of_range)
    {
      if (! intervals().contains_point(point)) return 0.0;

      double surface = 0.0;
      if (i_child->intervals().contains_point(point))
	{
	  surface += i_child->surface_at_point(point);
	  surface += time() - i_child->time();
	}
      return surface;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      if (! (idx < no_states()) )
	throw std::out_of_range("marker index out of range");

      // propagate value
      set_state(i_child,idx,state(idx));

      if (m.edge_has_mutation(time(),i_child->time()))
	{
	  set_state(i_child,idx,m.mutate_to(*i_child,idx));
	  i_child_mutating[idx] = true;
	}
      i_child->mutate_marker(idx,m);
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <genconversion time=\"" << time() << '"'
	 << " conversion_start=\"" << i_conversion_start << '"'
	 << " conversion_end=\"" << i_conversion_end << '"'
	 << " id=\"i_" << this << "\" haplotype=\"h_" << this << '"'
	 << "  is_inside=\"" << i_is_inside <<"\">" << std::endl
	 << "    <child ref=\"i_" << i_child << "\"/>" << std::endl
	 << "  </genconversion>" << std::endl; 
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      for (size_t i = 0; i < no_states(); ++i)
	if (i_child_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << i_child << "\"/> " << std::endl; 
    }

    Node *const i_child;
    std::valarray<bool> i_child_mutating;
    double i_conversion_start, i_conversion_end;
    bool i_is_inside;
  };
}


ARG::~ARG()
{
  std::vector<Node*>::iterator itr;
  for (itr = i_leaf_pool.begin(); itr != i_leaf_pool.end(); ++itr)
    delete *itr;
  for (itr = i_node_pool.begin(); itr != i_node_pool.end(); ++itr)
    delete *itr;
}

Node *ARG::leaf() throw()
{
  LeafNode *n = new LeafNode(i_conf);

#if 0 //OPTIMIZATION_1
  for (unsigned int i = 0; i < i_conf.no_markers(); ++i)
    {
      double marker_pos = i_conf.position(i);
      double start = std::max(0.0, marker_pos - 1e-5);
      double stop  = std::min(1.0, marker_pos + 1e-5);
      n->_intervals.add(start,stop,1);
    }
#else
  // the leaves covers the entire interval [0,1)
  n->i_intervals.add(0.0,1.0,1);
#endif

  i_leaf_pool.push_back(n);
  ++i_no_leaves;

  return n;
}

Node *ARG::coalescence(double time, Node *left, Node *right)
  throw(null_child)
{
  if (left == 0 or right == 0) throw null_child();

  // sort in retired and non-retired intervals
  std::vector<Interval> retired;
  Intervals non_retired;
  Intervals merged = left->intervals() | right->intervals();

#if 0
  std::cout << "coalescence -- left: " << left->intervals() << std::endl;
  std::cout << "coalescence -- right: " << right->intervals() << std::endl;
  std::cout << "coalescence -- merged: " << merged << std::endl;
#endif

  for (int i = 0; i < merged.size(); ++i)
    {
      if (merged.interval(i).leaf_contacts() < i_no_leaves)
	non_retired.add(merged.interval(i));
      else if (merged.interval(i).leaf_contacts() == i_no_leaves)
	retired.push_back(merged.interval(i));
      else
	assert(false);
    }

  CoalescentNode *n = new CoalescentNode(i_conf,time,left,right,non_retired);

  i_node_pool.push_back(n);

  std::vector<Interval>::const_iterator itr;
  for (itr = retired.begin(); itr != retired.end(); ++itr)
    {
#if EXPENSIVE_ASSERTS
      std::vector<RetiredInterval>::const_iterator jtr;
      for (jtr = i_retired_intervals.begin(); 
	   jtr != i_retired_intervals.end(); ++jtr)
	//assert(!jtr->overlaps(*itr));
	if (jtr->overlaps(*itr))
	  {
	    std::cout << "ERROR: " << *itr << " overlaps " << *jtr << std::endl;
	    assert(!jtr->overlaps(*itr));
	  }
#endif
      i_retired_intervals.push_back(RetiredInterval(*itr,n));
    }

  return n;
}

static inline bool contains_marker(const Configuration &conf,
				   const Interval &i)
{
  for (size_t m = 0; m < conf.no_markers(); ++m)
    if (i.contains_point(conf.position(m))) return true;
  return false;
}

static Intervals filter_contains_marker(const Intervals &intervals,
					const Configuration &conf)
{
  Intervals result;
  for (int i = 0; i < intervals.size(); ++i)
    {
      const Interval &it = intervals.interval(i);
      if (contains_marker(conf,it)) result.add(it);
    }
  return result;
}


ARG::node_pair_t ARG::recombination(double time, Node *child,
				    double cross_over_point)
  throw(null_child,Interval::interval_out_of_range,Interval::empty_interval)
{
  if (child == 0) throw null_child();

  if (cross_over_point <= child->intervals().first_point())
    return std::make_pair<Node*,Node*>(child,0);
  if (child->intervals().last_point() <= cross_over_point)
    return std::make_pair<Node*,Node*>(child,0);

  Intervals left  = child->intervals().copy(0.0,cross_over_point);
  left  = filter_contains_marker(left, i_conf);
  if (left.size() == 0) return std::make_pair<Node*,Node*>(child,0);

  Intervals right = child->intervals().copy(cross_over_point,1.0);
  right = filter_contains_marker(right, i_conf);
  if (right.size() == 0) return std::make_pair<Node*,Node*>(child,0);

#if 0
  std::cout << "recombination -- child: " << child->intervals() << std::endl;
  std::cout << "recombination -- left: " << left << std::endl;
  std::cout << "recombination -- right: " << right << std::endl;
#endif

  RecombinationNode *n1 = new RecombinationNode(i_conf,time,child,left,
						cross_over_point, true);
  RecombinationNode *n2 = new RecombinationNode(i_conf,time,child,right,
						cross_over_point, false);
  i_node_pool.push_back(n1); i_node_pool.push_back(n2);

  return std::make_pair(n1,n2);
}

ARG::node_pair_t ARG::gene_conversion(double time, Node *child,
				      double conversion_start,
				      double conversion_end)
  throw(null_child,Interval::interval_out_of_range,Interval::empty_interval)
{
  if (child == 0) throw null_child();

  if (conversion_start == conversion_end)
    return std::make_pair<Node*,Node*>(child,0); // empty interval...

  if (conversion_end <= child->intervals().first_point())
      return std::make_pair<Node*,Node*>(child,0);
  if (child->intervals().last_point() <= conversion_start)
      return std::make_pair<Node*,Node*>(child,0);


  Intervals left  = child->intervals().copy(0.0, conversion_start)
    + child->intervals().copy(conversion_end, 1.0);
  left  = filter_contains_marker(left, i_conf);
  if (left.size() == 0) return std::make_pair<Node*,Node*>(child,0);

  Intervals right =
    child->intervals().copy(conversion_start, conversion_end);
  right = filter_contains_marker(right, i_conf);
  if (right.size() == 0) return std::make_pair<Node*,Node*>(child,0);

#if 0
  std::cout << "gene-conversion -- left: " << left << std::endl;
  std::cout << "gene-conversion -- right: " << right << std::endl;
#endif


  GeneConversionNode *n1 = new GeneConversionNode(i_conf,time,child,left,
						  conversion_start,
						  conversion_end,
						  false);
  GeneConversionNode *n2 = new GeneConversionNode(i_conf,time,child,right,
						  conversion_start,
						  conversion_end,
						  true);
  i_node_pool.push_back(n1); i_node_pool.push_back(n2);

  return std::make_pair<Node*,Node*>(n1,n2);

}

namespace {
  using std::binary_function;
  struct starts_before : 
    public binary_function<const Interval&,const Interval&,bool> {
    bool operator () (const Interval &i1, const Interval &i2) const
    { return i1.start() < i2.start(); }
  };
};

void ARG::sort_retired_intervals()
{
  sort(i_retired_intervals.begin(), i_retired_intervals.end(),
       starts_before());
}


namespace {
  class interval_printer :
    public std::unary_function<void,const RetiredInterval&>
  {
  public:
    typedef void (RetiredInterval::*to_xml_t)(std::ostream &os) const;
    interval_printer(to_xml_t f, std::ostream &os) : i_f(f), i_os(os) {};
    void operator () (const RetiredInterval &ri) { (ri.*i_f)(i_os); }

  private:
    to_xml_t i_f;
    std::ostream &i_os;
  };

  class node_printer : public std::unary_function<void,const Node*>
  {
  public:
    typedef void (Node::*to_xml_t)(std::ostream &os) const;
    node_printer(to_xml_t f, std::ostream &os) : i_f(f), i_os(os) {};
    void operator () (const Node *n) { (n->*i_f)(i_os); }

  private:
    to_xml_t i_f;
    std::ostream &i_os;
  };
}

void ARG::to_xml(std::ostream &os, bool print_all_nodes) const
{
  std::string dtd = DTD_DIR"/coasim.dtd";
  const char *dtd_dir = getenv("COASIMDIR");
  if (dtd_dir) dtd = std::string(dtd_dir)+"/coasim.dtd";
  

  const std::vector<Node*>::const_iterator lpb = i_leaf_pool.begin();
  const std::vector<Node*>::const_iterator lpe = i_leaf_pool.end();
  const std::vector<Node*>::const_iterator npb = i_node_pool.begin();
  const std::vector<Node*>::const_iterator npe = i_node_pool.end();


  os << "<?xml version=\"1.0\"?>" << std::endl;
  os <<"<!DOCTYPE coasim SYSTEM \"" << dtd << "\"> " << std::endl;


  os << "<coasim>" << std::endl;

  // FIXME: configuration

  //out << "<coasim output_mode=\"" << output_mode << "\" leaf_nodes=\"" << leaf_nodes << "\" positions=\""<<pos_string<<"\" value_set=\""<<alp_string<<"\" Q=\""<<Q<<"\" G=\""<<G<<"\" rho=\""<<rho<<"\" mu=\""<<mu<<"\">" << std::endl;


  os << "  <markers>" << std::endl;
  for (size_t i = 0; i < i_conf.no_markers(); ++i)
    {
      os << "    <marker id=\"marker_" << i << "\">\n"
	 << "      <position>" << i_conf.position(i) << "</position>\n"
	 << "      <value-set>";

      for (size_t j = 0; j < i_conf.marker(i).size(); ++j)
	os << "<value>" << i_conf.marker(i).value(j) << "</value>";

      os << "</value-set>\n"
	 << "    </marker>" << std::endl;
  }
  os << "  </markers>" << std::endl;


  os << "  <haplotypes>" << std::endl;
  for_each(lpb,lpe, node_printer(&Node::haplotype_to_xml,os));
  if (print_all_nodes)
    for_each(npb,npe, node_printer(&Node::haplotype_to_xml,os));
  os << "  </haplotypes>" << std::endl;

  if (print_all_nodes)
    std::for_each(i_retired_intervals.begin(),
		  i_retired_intervals.end(),
		  interval_printer(&RetiredInterval::to_xml,os));

  std::for_each(lpb, lpe, node_printer(&Node::node_to_xml,os));
  if (print_all_nodes) 
    for_each(npb,npe,node_printer(&Node::node_to_xml,os));

  os << "  <mutations>" << std::endl;

  for_each(lpb,lpe, node_printer(&Node::mutation_to_xml,os));
  if (print_all_nodes)
    for_each(npb,npe, node_printer(&Node::mutation_to_xml,os));

  os << "  </mutations>" << std::endl
     << "</coasim>" << std::endl; 
}

namespace {
  class state_printer : public std::unary_function<void,const Node*>
  {
  public:
    state_printer(std::ostream &os) : i_os(os) {};
    void operator () (const Node *n) {
      for (unsigned int s = 0; s < n->no_states(); ++s)
	i_os << n->state(s) << ' ';
      i_os << '\n';
    }

  private:
    std::ostream &i_os;
  };
}

void ARG::to_text(std::ostream &os) const
{
  for_each(i_leaf_pool.begin(), i_leaf_pool.end(), state_printer(os));
}
