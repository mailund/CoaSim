/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#ifndef CORE__NODE_HH_INCLUDED
#define CORE__NODE_HH_INCLUDED

#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include "configuration.hh"
#endif
#ifndef CORE__INTERVAL_HH_INCLUDED
# include "interval.hh"
#endif
#ifndef CORE__RETIRED_INTERVAL_HH_INCLUDED
# include "retired_interval.hh"
#endif

#ifndef STDEXCEPT_INCLUDED
# include <stdexcept>
# define STDEXCEPT_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif
#ifndef IOSTREAM_INCLUDED
# include <iostream>
# define IOSTREAM_INCLUDED
#endif
#ifndef VALARRAY_INCLUDED
# include <valarray>
# define VALARRAY_INCLUDED
#endif

namespace core {

    class Marker;
    class Mutator;

    // -- Abstract class for ARG nodes --------------------------------------
    class Node 
    {
	// explicitly remove chance of copying
	Node(const Node&);
	Node &operator = (const Node&);


    public:
	Node(const Configuration &conf, double time) 
	    : i_time(time), i_states(-1,conf.no_markers()) 
	{}
	Node(const Configuration &conf, double time, const Intervals &i)
	    : i_time(time), i_intervals(i),  i_states(-1,conf.no_markers())
	{}
	virtual ~Node() {};

	double time() const { return i_time; }

	// the sub-intervals of the range [0,1) that connects this node to
	// a leaf node in the ARG -- if a point is not in one of these
	// intervals it is lost somewhere from here to the leaves.  As an
	// invariant, two points on the same interval correspond to the
	// same binary tree of the ARG.
	const Intervals &intervals() const { return i_intervals; }

	// calculate the "surface" (i.e. the total edge-length) of the
	// tree in point, with this node as root (zero if the point is not
	// in this node's intervals).
	virtual double surface_at_point(double point) const
	    throw(std::out_of_range) = 0;

	// Prints the local tree to a stream
	void print_tree_at_point(std::ostream &os, double point) const
	    throw(std::out_of_range) 
	{ print_tree_at_point(os, point, 0.0); }
	// Prints the local tree to a stream
	virtual void print_tree_at_point(std::ostream &os, double point,
					 double edge_length) const
	    throw(std::out_of_range) = 0;

	// Calculate the number of leaves hit by the binary tree in point
	// with root in this node.
	unsigned int leaves_at_point(double point) const throw(std::out_of_range)
	{ return intervals().leaf_contacts(point); }


	// FIXME: I am not sure this is the access-protection for these...
	void initialize_marker(unsigned int idx, const Marker &m);
	virtual void mutate_marker(unsigned int idx, Mutator &m) = 0;

  
	unsigned int no_states()  const { return i_states.size(); }
	int state(unsigned int s) const throw(std::out_of_range)
	{ if (i_states.size() <= s) throw std::out_of_range("s out of range");
	return i_states[s]; }

    protected:
	void set_state(unsigned int s, int val) 
	{ i_states[s] = val; }

	// hack to work around C++'s crappy "don't access protected through
	// other than this" protection...
	static void set_state(Node *n, unsigned int s, int val)
	{ n->set_state(s,val); }

    private:
	friend class ARG;
	virtual void node_to_xml(std::ostream &os)                const = 0;
	virtual void mutation_to_xml(std::ostream &os)            const = 0;
	void haplotype_to_xml(std::ostream &os)                   const;

	double    i_time;
	Intervals i_intervals;
	std::valarray<int> i_states;
    };


    class ARG
    {
    public:

	// Exception thrown if a node is created with a 0-child
	struct null_child : public std::logic_error {
	    null_child() : std::logic_error("null child.") {}
	};


	// -- Initialization and book-keeping -----------------------------------
	ARG(const Configuration &conf) : i_conf(conf), i_no_leaves(0) {}

	// Cleanup.  Destroying the ARG also deletes all nodes in it, so
	// don't keep any pointers to them around after this deletion.
	~ARG();


	// -- Factory methods for building the ARG ------------------------------
	Node *leaf()                                            throw();
	Node *coalescence(double time, Node *left, Node *right) throw(null_child);

	// these methods return a pair of new nodes, if two nodes were
	// actually created, or the child node (as the first element in
	// pair, the second being 0), if one of the nodes created would
	// otherwise be immediately retired
	typedef std::pair<Node*,Node*> node_pair_t;
	node_pair_t recombination(double time, Node *child,
				  double cross_over_point)
	    throw(null_child,Interval::interval_out_of_range,Interval::empty_interval);
	node_pair_t gene_conversion(double time, Node *child,
				    double conversion_start,
				    double conversion_end)
	    throw(null_child,Interval::interval_out_of_range,Interval::empty_interval);




	const std::vector<RetiredInterval> &retired_intervals() const
	{ return i_retired_intervals; }
	void sort_retired_intervals();

	unsigned int no_nodes() const
	{ return i_leaf_pool.size() + i_node_pool.size(); }

	void to_xml (std::ostream &os, bool print_all_nodes = false) const;
	void to_text(std::ostream &os) const;

	const std::vector<Node*> &leaves() const { return i_leaf_pool; }

    private:
	// disable these
	ARG(const ARG&);
	ARG &operator = (const ARG&);

	const Configuration &i_conf;
	size_t i_no_leaves;

	// pools of nodes -- FIXME: can be handled more efficiently...
	std::vector<Node*> i_leaf_pool;
	std::vector<Node*> i_node_pool;

	std::vector<RetiredInterval> i_retired_intervals;
    };

}

#endif
