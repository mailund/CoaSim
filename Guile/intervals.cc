/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "intervals.hh"
using namespace guile;

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
	    if (c != EOF) {
		char z = c;
		if (write (fd, &z, 1) != 1) {
		    return EOF;
		}
	    }
	    return c;
	}
	// write multiple characters
	virtual
	std::streamsize xsputn (const char* s,
				std::streamsize num) {
	    return write(fd,s,num);
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
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate p markers 100))) (intervals arg)))
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
  <prototype>(total-branch-length interval)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate p markers 100))) (intervals arg)))
(map total-branch-length intervals)</example>
  <description>
    <p>
     Returns the total tree branch length of the local tree of an interval.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

  SCM total_branch_length(SCM interval_smob)
  {
      SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
		 interval_smob, SCM_ARG1, "interval-end");
      IntervalData *interval_data 
                        = (IntervalData*) SCM_SMOB_DATA(interval_smob);
      return scm_make_real(interval_data->rinterval->surface());
  }

/* --<GUILE COMMENT>---------------------------------------------

<method name="interval->tree">
  <brief>Returns tree local to an interval.</brief>
  <prototype>(interval->tree interval)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate p markers 100))) (intervals arg)))
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
  <brief>Returns interval a local tree local covers.</brief>
  <prototype>(tree->interval tree)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate p markers 100))) (intervals arg)))
(define trees (map interval->tree intervals)) 
(define intervals2 (map tree->interval trees))</example>
  <description>
    <p>
      Returns interval a local tree local covers.  The function is the reverse
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

    scm_c_define_gsubr("interval->tree", 1, 0, 0, 
		       (scm_unused_struct*(*)())interval2tree);
    scm_c_define_gsubr("tree->interval", 1, 0, 0, 
		       (scm_unused_struct*(*)())tree2interval);
}


