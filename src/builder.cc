
#include "builder.hh"

#ifndef DIST_FUNCTIONS_HH_INCLUDED
# include "dist_funcs.hh"
#endif


namespace
{
  class TopNodeSet {
  public:
    TopNodeSet() {};

    // add a node to the set
    void push(Node *n)
    {
      _nodes.push_back(n);
    }

    // pop a random node from the set
    Node *pop()
    {
      size_t index = Distribution_functions::irand(_nodes.size());
      std::swap(_nodes[index], _nodes.back());
      Node *n = _nodes.back();
      _nodes.pop_back();
      return n;
    }

    size_t size() const { return _nodes.size(); }

  private:
    std::vector<Node*> _nodes;
  };

  inline double get_time_interval(const Configuration &conf,
				  double current_time,
				  unsigned int nodes_left)
  {
    using namespace Distribution_functions;

    if (fabs(conf.growth()) > 0.001) // enough growth?
      {
	double time1 = log(1.0+conf.growth()
			   *expdev(nodes_left,(nodes_left-1)/2)
			   *exp(-conf.growth()*current_time))/conf.growth();
	double time2 = expdev(nodes_left,conf.G()+conf.rho());
	return std::min(time1,time2);
      }
    else
      return expdev(nodes_left,(nodes_left-1)/2.+conf.G()+conf.rho());
  }
}


ARG * Builder::build(size_t no_leaf_nodes) const
{
  using namespace Distribution_functions;

  std::auto_ptr<ARG> arg(new ARG(_conf));
  TopNodeSet top_nodes;

  // initialize
  for (size_t i = 0; i < no_leaf_nodes; ++i)
    top_nodes.push(arg->leaf());

  // build tree
  double time = 0.0;
  while (top_nodes.size() > 1)
    {
      unsigned int k = top_nodes.size();
      time += get_time_interval(_conf,time,k);

      switch (uniform((k-1)/2.,_conf.G(), _conf.rho()))
	{
	case 0: // coalescent
	  {
	    Node *child1 = top_nodes.pop();
	    Node *child2 = top_nodes.pop();
	    Node *coa_node = arg->coalescence(time, child1, child2);
	    top_nodes.push(coa_node);
	  }
	  break;
	  
	case 1: // gene conversion
	  {
	    double point = uniform();
	    double length = random_sign()*expdev(_conf.Q());

	    double start = std::max(0.0, (length < 0) ? point+length : point);
	    double stop  = std::min(1.0, (length < 0) ? point : point+length);

	    // it *is* technically possible to randomly choose an
	    // empty length or hit one of the endpoints with the lengh
	    // reaching outside the interval -- although very
	    // unlikely.  If it happens, just pretend it didn't and
	    // move on -- this is the same effect as if we select a
	    // gene conversion outside an active interval.
	    if (stop-start <= 0.0) break;

	    Node *child = top_nodes.pop();
	    ARG::node_pair_t pair = arg->gene_conversion(time,child,
							 start,stop);
	    if (pair.second == 0) top_nodes.push(child);
	    else { top_nodes.push(pair.first); top_nodes.push(pair.second); }
	  }
	  break;

	case 2: // recombination
	  {
	    double cross_over_point = uniform();
	    Node *child = top_nodes.pop();
	    ARG::node_pair_t pair = arg->recombination(time,child,
						       cross_over_point);
	    if (pair.second == 0) top_nodes.push(child);
	    else { top_nodes.push(pair.first); top_nodes.push(pair.second); }
	  }
	  break;
	}

    }

  return arg.release();
}


