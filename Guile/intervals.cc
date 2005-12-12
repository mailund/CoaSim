/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "intervals.hh"
using namespace guile;

#ifndef GUILE_NODES_HH_INCLUDED
# include "nodes.hh"
#endif

#ifndef CORE__RETIRED_INTERVAL_HH_INCLUDED
# include <Core/retired_interval.hh>
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif



scm_t_bits guile::interval_tag;
scm_t_bits guile::local_tree_tag;

namespace {

    struct IntervalData {
	SCM arg;
	const core::RetiredInterval *rinterval;
	IntervalData(SCM arg, const core::RetiredInterval *rinterval)
	    : arg(arg), rinterval(rinterval)
	{
	}
	~IntervalData()
	{
	}
    };

    // right now, a tree is just represented by the interval, since
    // the actual tree is defined by it ... they are not exactly the
    // same, though, since you cannot use exactly the same scheme
    // functions on it -- they have different tags to make them work
    // polymorphic in some ways.
    struct TreeData {
	SCM arg;
	const core::RetiredInterval *rinterval;
	TreeData(SCM arg, const core::RetiredInterval *rinterval)
	    : arg(arg), rinterval(rinterval)
	{
	}
	~TreeData()
	{
	}
    };
    

    SCM mark_interval (SCM s_interval)
    {
	IntervalData *interval = (IntervalData*) SCM_SMOB_DATA(s_interval);
	scm_gc_mark(interval->arg);
	return SCM_BOOL_F;
    }

    size_t free_interval(SCM s_interval)
    {
	IntervalData *interval = (IntervalData*) SCM_SMOB_DATA(s_interval);
	interval->~IntervalData();
	scm_must_free(interval);
	return sizeof(IntervalData);
    }

    SCM mark_tree (SCM s_tree)
    {
	TreeData *tree = (TreeData*) SCM_SMOB_DATA(s_tree);
	scm_gc_mark(tree->arg);
	return SCM_BOOL_F;
    }

    size_t free_tree(SCM s_tree)
    {
	TreeData *tree = (TreeData*) SCM_SMOB_DATA(s_tree);
	tree->~TreeData();
	scm_must_free(tree);
	return sizeof(TreeData);
    }

    /* Cut from fdstream.hpp
     *
     *(C) Copyright Nicolai M. Josuttis 2001.
     * Permission to copy, use, modify, sell and distribute this software
     * is granted provided this copyright notice appears in all copies.
     * This software is provided "as is" without express or implied
     * warranty, and with no claim as to its suitability for any purpose.
     */

    class fdoutbuf : public std::streambuf {
    protected:
	int fd;    // file descriptor
    public:
	// constructor
	fdoutbuf (int _fd) : fd(_fd) {
	}
    protected:
	// write one character
	virtual int_type overflow (int_type c) {
#if 1 // FIXME: this won't work with the intalled g++ version :-(
	    if (c != EOF) {
		char z = c;
		if (write (fd, &z, 1) != 1) {
		    return EOF;
		}
	    }
#endif
	    return c;
	}
	// write multiple characters
	virtual
	std::streamsize xsputn (const char* s,
				std::streamsize num) {
#if 1 // FIXME: this won't work with the intalled g++ version :-(
	    return write(fd,s,num);
#endif
	    return 0;
	}
    };

    class fdostream : public std::ostream {
    protected:
	fdoutbuf buf;
    public:
	fdostream (int fd) : std::ostream(0), buf(fd) {
	    rdbuf(&buf);
	}
    };

    int print_tree (SCM tree_smob, SCM port, scm_print_state *pstate)
    {
	TreeData *tree_data = (TreeData*) SCM_SMOB_DATA(tree_smob);
	double start = tree_data->rinterval->start();
	core::Node *node = tree_data->rinterval->top_node();

	int fd = scm_num2int(scm_fileno(port),SCM_ARG2,"");
	//__gnu_cxx::stdio_filebuf<char> obuf(dup(fd), std::ios::out);
	//std::ostream os(&obuf);
	fdostream os(fd);
	os.precision(12);	// float precision
	node->print_tree_at_point(os, start);
	os << std::endl;

	return 1;
    }

    /* --<GUILE COMMENT>---------------------------------------------

<method name="interval-start">
  <brief>Returns the start position of an interval.</brief>
  <prototype>(interval-start interval)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate p markers 100))) (intervals arg)))
(map interval-start intervals)</example>
  <description>
    <p>
     Returns the start position of an interval.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
    SCM interval_start(SCM interval_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
		   interval_smob, SCM_ARG1, "interval-start");
	IntervalData *interval_data 
	                = (IntervalData*) SCM_SMOB_DATA(interval_smob);
	return scm_make_real(interval_data->rinterval->start());
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="interval-end">
  <brief>Returns the end position of an interval.</brief>
  <prototype>(interval-end interval)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate markers 100 :rho 400))) (intervals arg)))
(map interval-end intervals)</example>
  <description>
    <p>
     Returns the end position of an interval.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM interval_end(SCM interval_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
		   interval_smob, SCM_ARG1, "interval-end");
	IntervalData *interval_data
                         = (IntervalData*) SCM_SMOB_DATA(interval_smob);
	return scm_make_real(interval_data->rinterval->end());
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="total-branch-length">
  <brief>Returns the total tree branch length of the local tree of an interval.</brief>
  <prototype>(total-branch-length interval-or-tree)</prototype>
  <example>  (use-modules (coasim rand))
 (define markers (make-random-snp-markers 10 0.1 0.9))
 (define intervals (intervals (simulate markers 100 :rho 400)))
 (map total-branch-length intervals)</example>
  <description>
    <p>
     Returns the total tree branch length of the local tree of an interval.
     The argument to the function can be either a local interval, as returned
     by the intervals function, or a local tree, as returned by the local-trees
     function.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

  SCM total_branch_length(SCM smob)
  {
      double surface = 0.0;
      if (SCM_SMOB_PREDICATE(guile::interval_tag, smob))
          {
	      IntervalData *interval_data = (IntervalData*)SCM_SMOB_DATA(smob);
	      surface = interval_data->rinterval->surface();
	  }
      else if (SCM_SMOB_PREDICATE(guile::local_tree_tag, smob))
          {
	      TreeData *tree_data = (TreeData*) SCM_SMOB_DATA(smob);
	      surface = tree_data->rinterval->surface();
	  }
      else
         {
	     scm_wrong_type_arg("total-branch-length", 1, smob);
         }
      

      return scm_make_real(surface);
  }

/* --<GUILE COMMENT>---------------------------------------------

<method name="tree-height">
  <brief>Returns the height of the local tree of an interval.</brief>
  <prototype>(tree-height interval-or-tree)</prototype>
  <example> (use-modules (coasim rand))
 (define markers (make-random-snp-markers 10 0.1 0.9))
 (define intervals (local-trees (simulate markers 100 :rho 400)))
 (map tree-height intervals)</example>
  <description>
    <p>
     Returns the height of the local tree of an interval.
     The argument to the function can be either a local interval, as returned
     by the intervals function, or a local tree, as returned by the local-trees
     function.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

  SCM tree_height(SCM smob)
  {
      double height = 0.0;
      if (SCM_SMOB_PREDICATE(guile::interval_tag, smob))
          {
	      IntervalData *interval_data = (IntervalData*)SCM_SMOB_DATA(smob);
	      height = interval_data->rinterval->top_node()->time();
	  }
      else if (SCM_SMOB_PREDICATE(guile::local_tree_tag, smob))
          {
	      TreeData *tree_data = (TreeData*) SCM_SMOB_DATA(smob);
	      height = tree_data->rinterval->top_node()->time();
	  }
      else
         {
	     scm_wrong_type_arg("tree-height", 1, smob);
         }
      

      return scm_make_real(height);
  }


/* --<GUILE COMMENT>---------------------------------------------

<method name="root">
  <brief>Returns the root of the local tree of an interval.</brief>
  <prototype>(root interval-or-tree)</prototype>
  <example> (use-modules (coasim rand))
 (define markers (make-random-snp-markers 10 0.1 0.9))
 (define trees (local-trees (simulate markers 100 :rho 400)))
 (map root trees)</example>
  <description>
    <p>
     Returns the root node of the local tree of an interval.
     The argument to the function can be either a local interval, as returned
     by the intervals function, or a local tree, as returned by the local-trees
     function.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

  SCM root(SCM smob)
  {
      if (SCM_SMOB_PREDICATE(guile::interval_tag, smob))
          {
	      IntervalData *interval_data = (IntervalData*)SCM_SMOB_DATA(smob);
	      return guile::wrap_node(interval_data->arg,
				      interval_data->rinterval->top_node());
	  }
      else if (SCM_SMOB_PREDICATE(guile::local_tree_tag, smob))
          {
	      TreeData *tree_data = (TreeData*) SCM_SMOB_DATA(smob);
	      return guile::wrap_node(tree_data->arg,
				      tree_data->rinterval->top_node());
	  }
      else
         {
	     scm_wrong_type_arg("tree-height", 1, smob);
         }
      
      // not reached
      assert(false);
      return SCM_EOL;
  }


/* --<GUILE COMMENT>---------------------------------------------

<method name="interval->tree">
  <brief>Returns tree local to an interval.</brief>
  <prototype>(interval->tree interval)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (intervals (simulate markers 100 :rho 400)))
(define trees (map interval->tree intervals)) </example>
  <description>
    <p>Returns the tree local to an interval.</p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
    SCM interval2tree(SCM interval_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
		   interval_smob, SCM_ARG1, "interval->tree");

	IntervalData *interval_data
	    = (IntervalData*) SCM_SMOB_DATA(interval_smob);
	void *mem = scm_must_malloc(sizeof(TreeData), "interval->tree");
	TreeData *tree = new(mem)TreeData(interval_data->arg,
					  interval_data->rinterval);
	SCM_RETURN_NEWSMOB(guile::local_tree_tag, tree);
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="tree->interval">
  <brief>Returns interval a local tree covers.</brief>
  <prototype>(tree->interval tree)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate markers 100 :rho 400))) (intervals arg)))
(define trees (map interval->tree intervals)) 
(define intervals2 (map tree->interval trees))</example>
  <description>
    <p>
      Returns interval a local tree covers.  The function is the reverse
      of interval->tree.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
    SCM tree2interval(SCM tree_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(guile::local_tree_tag, tree_smob),
		   tree_smob, SCM_ARG1, "tree->interval");

	TreeData *tree_data = (TreeData*) SCM_SMOB_DATA(tree_smob);
	void *mem = scm_must_malloc(sizeof(IntervalData), "tree->interval");
	IntervalData *interval = new(mem)IntervalData(tree_data->arg,
						      tree_data->rinterval);
	SCM_RETURN_NEWSMOB(guile::interval_tag, interval);
    }


}



SCM guile::wrap_interval(SCM arg, const core::RetiredInterval *rinterval)
{
    void *mem = scm_must_malloc(sizeof(IntervalData), "*interval*");
    IntervalData *interval = new(mem)IntervalData(arg,rinterval);
    SCM_RETURN_NEWSMOB(guile::interval_tag, interval);
}



void
guile::install_intervals()
{
    // FIXME: add printing of intervals
    guile::interval_tag = scm_make_smob_type("interval", sizeof(IntervalData));
    scm_set_smob_mark(guile::interval_tag, mark_interval);
    scm_set_smob_free(guile::interval_tag, free_interval);

    guile::local_tree_tag = scm_make_smob_type("tree", sizeof(TreeData));
    scm_set_smob_mark (guile::local_tree_tag, mark_tree);
    scm_set_smob_free (guile::local_tree_tag, free_tree);
    scm_set_smob_print(guile::local_tree_tag, print_tree);

    scm_c_define_gsubr("interval-start", 1, 0, 0, 
		       (scm_unused_struct*(*)())interval_start);
    scm_c_define_gsubr("interval-end", 1, 0, 0, 
		       (scm_unused_struct*(*)())interval_end);
    scm_c_define_gsubr("total-branch-length", 1, 0, 0, 
		       (scm_unused_struct*(*)())total_branch_length);
    scm_c_define_gsubr("tree-height", 1, 0, 0, 
		       (scm_unused_struct*(*)())tree_height);
    scm_c_define_gsubr("root", 1, 0, 0, 
		       (scm_unused_struct*(*)())root);

    scm_c_define_gsubr("interval->tree", 1, 0, 0, 
		       (scm_unused_struct*(*)())interval2tree);
    scm_c_define_gsubr("tree->interval", 1, 0, 0, 
		       (scm_unused_struct*(*)())tree2interval);
}


