
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

#if EXPENSIVE_ASSERTS
# include <fstream>
#endif

void Node::haplotype_to_xml(std::ostream &os) const
{
  os << "    <haplotype id=\"h_" << this << "\"> " << std::endl;
  for (unsigned int i = 0; i < _states.size(); ++i){
    os << "      <loci marker_ref=\"marker_" << i << "\">";
    os << "<allele>" << _states[i] << "</allele>";
    os << "</loci>" << std::endl;
  }
  os << "    </haplotype>" << std::endl;
}

void Node::initialize_marker(unsigned int idx, const Marker &m)
{
  if (_states.size() <= idx) 
    throw std::out_of_range("marker index out of range");
  _states[idx] = m.default_value();
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
      : Node(conf,time,is), _left(left), _right(right),
	_left_mutating(false,conf.no_markers()),
	_right_mutating(false,conf.no_markers()),
	_conf(conf)
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
      if (_left->intervals().contains_point(point))
	{
	  surface += _left->surface_at_point(point);
	  surface += time() - _left->time();
	}
      if (_right->intervals().contains_point(point))
	{
	  surface += _right->surface_at_point(point);
	  surface += time() - _right->time();
	}
      return surface;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      if (! (idx < no_states()) )
	throw std::out_of_range("marker index out of range");

      double point = _conf.position(idx);

      if (_left->intervals().contains_point(point))
	{
	  // propagate value
	  set_state(_left,idx,state(idx));
	  if (m.edge_has_mutation(time(),_left->time()))
	    {
	      set_state(_left, idx, m.mutate_to(*_left,idx));
	      _left_mutating[idx] = true;
	    }
	  _left->mutate_marker(idx,m);
	}


      if (_right->intervals().contains_point(point))
	{
	  set_state(_right,idx,state(idx));
	  if (m.edge_has_mutation(time(),_right->time()))
	    {
	      set_state(_right, idx, m.mutate_to(*_right,idx));
	      _right_mutating[idx] = true;
	    }
	  _right->mutate_marker(idx,m);
	}
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <coalescent time=\"" << time() << "\" id=\"i_" << this << '"'
	 << " haplotype=\"h_" << this << "\">" << std::endl;
      os << "    <child ref=\"i_" << _left << "\"/>" << std::endl;
      os << "    <child ref=\"i_" << _right << "\"/>" << std::endl;
      os << "  </coalescent>" << std::endl; 
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      for (size_t i = 0; i < no_states(); ++i){
	if (_left_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << _left << "\"/> "
	     << std::endl;
	else if (_right_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << _right << "\"/> "
	     << std::endl;
      }
    }

    Node *const _left;
    Node *const _right;
    std::valarray<bool> _left_mutating;
    std::valarray<bool> _right_mutating;
    const Configuration &_conf;
  };
  
  class RecombinationNode : public Node
  {
    friend ARG::node_pair_t ARG::recombination(double,Node*,double);
    RecombinationNode(const Configuration &conf,
		      double time, Node *child, const Intervals &is,
		      double cross_over_point, bool is_left)
      : Node(conf,time,is), _child(child),
	_child_mutating(false,conf.no_markers()),
	_cross_over_point(cross_over_point), _is_left(is_left)
    {}

    virtual double surface_at_point(double point) const
      throw(std::out_of_range)
    {
      if (! intervals().contains_point(point)) return 0.0;
      double surface = 0.0;
      if (_child->intervals().contains_point(point))
	{
	  surface += _child->surface_at_point(point);
	  surface += time() - _child->time();
	}
      return surface;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      if (! (idx < no_states()) )
	throw std::out_of_range("marker index out of range");

      // propagate value
      set_state(_child,idx,state(idx));

      if (m.edge_has_mutation(time(),_child->time()))
	{
	  set_state(_child,idx,m.mutate_to(*_child,idx));
	  _child_mutating[idx] = true;
	}
      _child->mutate_marker(idx,m);
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <recombination time=\"" << time() << '"'
	 << " crossover=\"" << _cross_over_point << '"'
	 << " id=\"i_" << this << "\" haplotype=\"h_" << this << '"'
	 << " is_left=\"" << _is_left << "\">" << std::endl
	 << "    <child ref=\"i_" << _child << "\"/>" << std::endl
	 << "  </recombination>" << std::endl; 
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      for (size_t i = 0; i < no_states(); ++i)
	if (_child_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << _child << "\"/> " << std::endl; 
    }

    Node *const _child;
    std::valarray<bool> _child_mutating;
    double _cross_over_point;
    bool _is_left;
  };

  class GeneConversionNode : public Node
  {
    friend ARG::node_pair_t ARG::gene_conversion(double,Node*,double,double);
    GeneConversionNode(const Configuration &conf,
		       double time, Node *child, const Intervals &is,
		       double conversion_start, double conversion_end,
		       bool is_inside)
      : Node(conf,time,is), _child(child),
	_child_mutating(false,conf.no_markers()),
	_conversion_start(conversion_start), _conversion_end(conversion_end),
	_is_inside(is_inside)
    {}

    virtual double surface_at_point(double point) const
      throw(std::out_of_range)
    {
      if (! intervals().contains_point(point)) return 0.0;
      double surface = 0.0;
      if (_child->intervals().contains_point(point))
	{
	  surface += _child->surface_at_point(point);
	  surface += time() - _child->time();
	}
      return surface;
    }

    virtual void mutate_marker(unsigned int idx, Mutator &m)
    {
      if (! (idx < no_states()) )
	throw std::out_of_range("marker index out of range");

      // propagate value
      set_state(_child,idx,state(idx));

      if (m.edge_has_mutation(time(),_child->time()))
	{
	  set_state(_child,idx,m.mutate_to(*_child,idx));
	  _child_mutating[idx] = true;
	}
      _child->mutate_marker(idx,m);
    }

    virtual void node_to_xml(std::ostream &os) const
    {
      os << "  <genconversion time=\"" << time() << '"'
	 << " conversion_start=\"" << _conversion_start << '"'
	 << " conversion_end=\"" << _conversion_end << '"'
	 << " id=\"i_" << this << "\" haplotype=\"h_" << this << '"'
	 << "  is_inside=\"" << _is_inside <<"\">" << std::endl
	 << "    <child ref=\"i_" << _child << "\"/>" << std::endl
	 << "  </genconversion>" << std::endl; 
    }

    virtual void mutation_to_xml(std::ostream &os) const
    {
      for (size_t i = 0; i < no_states(); ++i)
	if (_child_mutating[i])
	  os << "    <mutation marker_ref=\"marker_" << i << '"'
	     << " parent_ref=\"i_" << this << '"'
	     << " child_ref=\"i_" << _child << "\"/> " << std::endl; 
    }

    Node *const _child;
    std::valarray<bool> _child_mutating;
    double _conversion_start, _conversion_end;
    bool _is_inside;
  };
}


ARG::~ARG()
{
  std::vector<Node*>::iterator itr;
  for (itr = _leaf_pool.begin(); itr != _leaf_pool.end(); ++itr)
    delete *itr;
  for (itr = _node_pool.begin(); itr != _node_pool.end(); ++itr)
    delete *itr;
}

Node *ARG::leaf() throw()
{
  LeafNode *n = new LeafNode(_conf);

#if 0 //OPTIMIZATION_1
  for (unsigned int i = 0; i < _conf.no_markers(); ++i)
    {
      double marker_pos = _conf.position(i);
      double start = std::max(0.0, marker_pos - 1e-5);
      double stop  = std::min(1.0, marker_pos + 1e-5);
      n->_intervals.add(start,stop,1);
    }
#else
  // the leaves covers the entire interval [0,1)
  n->_intervals.add(0.0,1.0,1);
#endif

  _leaf_pool.push_back(n);
  ++_no_leaves;


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
      if (merged.interval(i).leaf_contacts() < _no_leaves)
	non_retired.add(merged.interval(i));
      else if (merged.interval(i).leaf_contacts() == _no_leaves)
	retired.push_back(merged.interval(i));
      else
	assert(false);
    }

  CoalescentNode *n = new CoalescentNode(_conf,time,left,right,non_retired);

  _node_pool.push_back(n);

  std::vector<Interval>::const_iterator itr;
  for (itr = retired.begin(); itr != retired.end(); ++itr)
    {
#if EXPENSIVE_ASSERTS
      std::vector<RetiredInterval>::const_iterator jtr;
      for (jtr = _retired_intervals.begin(); 
	   jtr != _retired_intervals.end(); ++jtr)
	//assert(!jtr->overlaps(*itr));
	if (jtr->overlaps(*itr))
	  {
	    std::cout << "ERROR: " << *itr << " overlaps " << *jtr << std::endl;
	    assert(!jtr->overlaps(*itr));
	  }
#endif
      _retired_intervals.push_back(RetiredInterval(*itr,n));
    }

  return n;
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
  Intervals right = child->intervals().copy(cross_over_point,1.0);

#if 0
  std::cout << "recombination -- child: " << child->intervals() << std::endl;
  std::cout << "recombination -- left: " << left << std::endl;
  std::cout << "recombination -- right: " << right << std::endl;
#endif


  // FIXME: we could optimize here by not creating intervals without markers

  RecombinationNode *n1 = new RecombinationNode(_conf,time,child,left,
						cross_over_point, true);
  RecombinationNode *n2 = new RecombinationNode(_conf,time,child,right,
						cross_over_point, false);
  _node_pool.push_back(n1); _node_pool.push_back(n2);

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

#if 0
  if (conversion_end <= child->intervals().first_point())
      return std::make_pair<Node*,Node*>(child,0);
  if (child->intervals().last_point() <= conversion_start)
      return std::make_pair<Node*,Node*>(child,0);
#else
  Interval conv(conversion_start,conversion_end,0);
  if (!child->intervals().overlaps(conv))
    return std::make_pair<Node*,Node*>(child,0);
#endif


  // FIXME: we could optimize here by not creating intervals without markers

#if 0
  std::cout << "gene-conversion -- interval: "
	    << '[' << conversion_start << ',' << conversion_end << ")\n";
  std::cout << "gene-conversion -- child: " << child->intervals() << std::endl;
#endif  

  Intervals left  = child->intervals().copy(0.0, conversion_start)
    + child->intervals().copy(conversion_end, 1.0);
  Intervals right =
    child->intervals().copy(conversion_start, conversion_end);

#if 0
  std::cout << "gene-conversion -- left: " << left << std::endl;
  std::cout << "gene-conversion -- right: " << right << std::endl;
#endif


  GeneConversionNode *n1 = new GeneConversionNode(_conf,time,child,left,
						  conversion_start,
						  conversion_end,
						  false);
  GeneConversionNode *n2 = new GeneConversionNode(_conf,time,child,right,
						  conversion_start,
						  conversion_end,
						  true);
  _node_pool.push_back(n1); _node_pool.push_back(n2);

  return std::make_pair<Node*,Node*>(n1,n2);

}

namespace {
  class interval_printer :
    public std::unary_function<void,const RetiredInterval&>
  {
  public:
    typedef void (RetiredInterval::*to_xml_t)(std::ostream &os) const;
    interval_printer(to_xml_t f, std::ostream &os) : _f(f), _os(os) {};
    void operator () (const RetiredInterval &ri) { (ri.*_f)(_os); }

  private:
    to_xml_t _f;
    std::ostream &_os;
  };

  class node_printer : public std::unary_function<void,const Node*>
  {
  public:
    typedef void (Node::*to_xml_t)(std::ostream &os) const;
    node_printer(to_xml_t f, std::ostream &os) : _f(f), _os(os) {};
    void operator () (const Node *n) { (n->*_f)(_os); }

  private:
    to_xml_t _f;
    std::ostream &_os;
  };
}

void ARG::to_xml(std::ostream &os) const
{
  std::string dtd = "coasim.dtd"; 	// FIXME: set dtd?

  const std::vector<Node*>::const_iterator lpb = _leaf_pool.begin();
  const std::vector<Node*>::const_iterator lpe = _leaf_pool.end();
  const std::vector<Node*>::const_iterator npb = _node_pool.begin();
  const std::vector<Node*>::const_iterator npe = _node_pool.end();


  os << "<?xml version=\"1.0\"?>" << std::endl;
  os <<"<!DOCTYPE coasim SYSTEM \"" << dtd << "\"> " << std::endl;


  os << "<coasim>" << std::endl;

  // FIXME: configuration

  //out << "<coasim output_mode=\"" << output_mode << "\" leaf_nodes=\"" << leaf_nodes << "\" positions=\""<<pos_string<<"\" value_set=\""<<alp_string<<"\" Q=\""<<Q<<"\" G=\""<<G<<"\" rho=\""<<rho<<"\" mu=\""<<mu<<"\">" << std::endl;


  os << "  <markers>" << std::endl;
  for (size_t i = 0; i < _conf.no_markers(); ++i)
    {
      os << "    <marker id=\"marker_" << i << "\">\n"
	 << "      <position>" << _conf.position(i) << "</position>\n"
	 << "      <value-set>";

      for (size_t j = 0; j < _conf.marker(i).size(); ++j)
	os << "<value>" << _conf.marker(i).value(j) << "</value>";

      os << "</value-set>\n"
	 << "    </marker>" << std::endl;
  }
  os << "  </markers>" << std::endl;


  os << "  <haplotypes>" << std::endl;
  for_each(lpb,lpe, node_printer(&Node::haplotype_to_xml,os));
  if (_conf.print_all_nodes())
    for_each(npb,npe, node_printer(&Node::haplotype_to_xml,os));
  os << "  </haplotypes>" << std::endl;

  std::for_each(_retired_intervals.begin(),
		_retired_intervals.end(),
		interval_printer(&RetiredInterval::to_xml,os));

  std::for_each(lpb, lpe, node_printer(&Node::node_to_xml,os));
  if (_conf.print_all_nodes()) 
    for_each(npb,npe,node_printer(&Node::node_to_xml,os));

  os << "  <mutations>" << std::endl;

  for_each(lpb,lpe, node_printer(&Node::mutation_to_xml,os));
  if (_conf.print_all_nodes())
    for_each(npb,npe, node_printer(&Node::mutation_to_xml,os));

  os << "  </mutations>" << std::endl
     << "</coasim>" << std::endl; 
}


