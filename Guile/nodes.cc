/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "nodes.hh"

#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif

using namespace guile;

scm_t_bits guile::leaf_node_tag;
scm_t_bits guile::coalescent_node_tag;
scm_t_bits guile::recombination_node_tag;
scm_t_bits guile::gene_conversion_node_tag;

namespace {
    struct LeafNode {
	SCM arg;
	const core::LeafNode *node;

	LeafNode(SCM arg, const core::LeafNode *node)
	    : arg(arg), node(node)
	{
	}
	~LeafNode()
	{
	}
    };


    SCM
    mark_leaf_node (SCM s_leaf_node)
    {
	LeafNode *leaf_node = (LeafNode*) SCM_SMOB_DATA(s_leaf_node);
	if (leaf_node->arg != SCM_EOL) scm_gc_mark(leaf_node->arg);
	return SCM_BOOL_F;
    }

    size_t
    free_leaf_node(SCM s_leaf_node)
    {
	LeafNode *leaf_node = (LeafNode*) SCM_SMOB_DATA(s_leaf_node);
	leaf_node->~LeafNode();
	scm_must_free(leaf_node);
	return sizeof(LeafNode);
    }

    struct CoalescentNode {
	SCM arg;
	const core::CoalescentNode *node;

	CoalescentNode(SCM arg, const core::CoalescentNode *node)
	    : arg(arg), node(node)
	{
	}
	~CoalescentNode()
	{
	}
    };



    SCM
    mark_coalescent_node (SCM s_coalescent_node)
    {
	CoalescentNode *coalescent_node = (CoalescentNode*) SCM_SMOB_DATA(s_coalescent_node);
	if (coalescent_node->arg != SCM_EOL) scm_gc_mark(coalescent_node->arg);

	return SCM_BOOL_F;
    }

    size_t
    free_coalescent_node(SCM s_coalescent_node)
    {
	CoalescentNode *coalescent_node = (CoalescentNode*) SCM_SMOB_DATA(s_coalescent_node);
	coalescent_node->~CoalescentNode();
	scm_must_free(coalescent_node);

	return sizeof(CoalescentNode);
    }

    struct RecombinationNode {
	SCM arg;
	const core::RecombinationNode *node;

	RecombinationNode(SCM arg, const core::RecombinationNode *node)
	    : arg(arg), node(node)
	{
	}
	~RecombinationNode()
	{
	}
    };


    SCM
    mark_recombination_node (SCM s_recombination_node)
    {
	RecombinationNode *recombination_node = (RecombinationNode*) SCM_SMOB_DATA(s_recombination_node);
	if (recombination_node->arg != SCM_EOL) 
	    scm_gc_mark(recombination_node->arg);
	return SCM_BOOL_F;
    }

    size_t
    free_recombination_node(SCM s_recombination_node)
    {
	RecombinationNode *recombination_node = (RecombinationNode*) SCM_SMOB_DATA(s_recombination_node);
	recombination_node->~RecombinationNode();
	scm_must_free(recombination_node);
	return sizeof(RecombinationNode);
    }

    struct GeneConversionNode {
	SCM arg;
	const core::GeneConversionNode *node;

	GeneConversionNode(SCM arg, const core::GeneConversionNode *node)
	    : arg(arg), node(node)
	{
	}
	~GeneConversionNode()
	{
	}
    };


    SCM
    mark_gene_conversion_node (SCM s_gene_conversion_node)
    {
	GeneConversionNode *gene_conversion_node = (GeneConversionNode*) SCM_SMOB_DATA(s_gene_conversion_node);
	if (gene_conversion_node->arg != SCM_EOL) 
	    scm_gc_mark(gene_conversion_node->arg);
	return SCM_BOOL_F;
    }

    size_t
    free_gene_conversion_node(SCM s_gene_conversion_node)
    {
	GeneConversionNode *gene_conversion_node = (GeneConversionNode*) SCM_SMOB_DATA(s_gene_conversion_node);
	gene_conversion_node->~GeneConversionNode();
	scm_must_free(gene_conversion_node);
	return sizeof(GeneConversionNode);
    }

}

SCM
guile::wrap_leaf_node(SCM arg, const core::LeafNode *node)
{
    void *mem = scm_must_malloc(sizeof(LeafNode), "*leaf-node*");
    LeafNode *n = new(mem)LeafNode(arg,node);
    SCM_RETURN_NEWSMOB(guile::leaf_node_tag, n);
}

SCM
guile::wrap_coalescent_node(SCM arg, const core::CoalescentNode *node)
{
    void *mem = scm_must_malloc(sizeof(CoalescentNode), "*coalescent-node*");
    CoalescentNode *n = new(mem)CoalescentNode(arg,node);
    SCM_RETURN_NEWSMOB(guile::coalescent_node_tag, n);
}

SCM
guile::wrap_recombination_node(SCM arg, const core::RecombinationNode *node)
{
    void *mem = scm_must_malloc(sizeof(RecombinationNode),
				"*recombination-node*");
    RecombinationNode *n = new(mem)RecombinationNode(arg,node);
    SCM_RETURN_NEWSMOB(guile::recombination_node_tag, n);
}

SCM
guile::wrap_gene_conversion_node(SCM arg, const core::GeneConversionNode *node)
{
    void *mem = scm_must_malloc(sizeof(GeneConversionNode),
				"*gene_conversion-node*");
    GeneConversionNode *n = new(mem)GeneConversionNode(arg,node);
    SCM_RETURN_NEWSMOB(guile::gene_conversion_node_tag, n);
}

namespace {

    
/* --<GUILE COMMENT>---------------------------------------------

<method name="event-time">
  <brief>Returns the time of the event represented by a node.</brief>
  <prototype>(event-time node)</prototype>
  <example>(define coa-times '())
(define (coa-cb n k) (set! coa-times (cons (event-time n) coa-times)))
(define rc-times '())
(define (rc-cb n1 n2) (set! rc-times (cons (event-time n1) rc-times)))
(simulate markers no-leaves :rho 400
	  :coalescence-callback   coa-cb 
	  :recombination-callback rc-cb) </example>
  <description>
    <p>Returns the time of the event represented by a node.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */


    SCM event_time(SCM node_smob)
    {
	if (SCM_SMOB_PREDICATE(guile::leaf_node_tag, node_smob))
	    {
		return scm_make_real(0.0);
	    }
	else if (SCM_SMOB_PREDICATE(guile::coalescent_node_tag, node_smob))
	    {
		CoalescentNode *node 
		    = (CoalescentNode*)SCM_SMOB_DATA(node_smob);
		return scm_make_real(node->node->time());
	    }
	else if (SCM_SMOB_PREDICATE(guile::recombination_node_tag, node_smob))
	    {
		RecombinationNode *node 
		    = (RecombinationNode*)SCM_SMOB_DATA(node_smob);
		return scm_make_real(node->node->time());
	    }
	else if (SCM_SMOB_PREDICATE(guile::gene_conversion_node_tag, node_smob))
	    {
		GeneConversionNode *node 
		    = (GeneConversionNode*)SCM_SMOB_DATA(node_smob);
		return scm_make_real(node->node->time());
	    }

	scm_throw(scm_str2symbol("not-a-node-error"), SCM_EOL);
	return SCM_EOL;
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="recombination-point">
  <brief>Returns the recombination point of a recombination node.</brief>
  <prototype>(recombination-point recombination-node)</prototype>
  <example>(define rc-points '())
(define (rc-cb n1 n2 k) (set! rc-points (cons (recombination-point n1) rc-points)))
(simulate markers no-leaves :rho 400 :recombination-callback rc-cb) </example>
  <description>
    <p>
      Returns the recombination point of a recombination node.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */


    SCM recombination_point(SCM node_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(recombination_node_tag, node_smob),
		   node_smob, SCM_ARG1, "recombination-point");
	
	RecombinationNode *node 
	    = (RecombinationNode*)SCM_SMOB_DATA(node_smob);
	return scm_make_real(node->node->cross_over_point());
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="gene-conversion-start">
  <brief>Returns the start point of a gene conversion.</brief>
  <prototype>(gene-conversion-start gene-conversion-node)</prototype>
  <example>(define gc-start '())
(define (gc-cb n1 n2 k) (set! gc-start (cons (gene-conversion-start n1) gc-start)))
(simulate markers no-leaves :gamma 10 :Q 0.2 :geneconversion-callback gc-cb) </example>
  <description>
    <p>
      Returns the start point of a gene conversion.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM gene_conversion_start(SCM node_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(gene_conversion_node_tag, node_smob),
		   node_smob, SCM_ARG1, "gene-conversion-start");
	
	GeneConversionNode *node 
	    = (GeneConversionNode*)SCM_SMOB_DATA(node_smob);
	return scm_make_real(node->node->conversion_start());
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="gene-conversion-end">
  <brief>Returns the end point of a gene conversion.</brief>
  <prototype>(gene-conversion-end gene-conversion-node)</prototype>
  <example>(define gc-end '())
(define (gc-cb n1 n2 k) (set! gc-end (cons (gene-conversion-end n1) gc-end)))
(simulate markers no-leaves :gamma 10 :Q 0.2 :geneconversion-callback gc-cb) </example>
  <description>
    <p>
      Returns the end point of a gene conversion.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM gene_conversion_end(SCM node_smob)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(gene_conversion_node_tag, node_smob),
		   node_smob, SCM_ARG1, "gene-conversion-end");
	
	GeneConversionNode *node 
	    = (GeneConversionNode*)SCM_SMOB_DATA(node_smob);
	return scm_make_real(node->node->conversion_end());
    }

}

void
guile::install_nodes()
{
    // FIXME: add printing 

    guile::leaf_node_tag = scm_make_smob_type("leaf-node",
					      sizeof(LeafNode));
    scm_set_smob_mark(guile::leaf_node_tag, mark_leaf_node);
    scm_set_smob_free(guile::leaf_node_tag, free_leaf_node);

    guile::coalescent_node_tag
	= scm_make_smob_type("coalescent-node", sizeof(CoalescentNode));
    scm_set_smob_mark (guile::coalescent_node_tag, mark_coalescent_node);
    scm_set_smob_free (guile::coalescent_node_tag, free_coalescent_node);

    guile::recombination_node_tag
	= scm_make_smob_type("recombination-node", sizeof(RecombinationNode));
    scm_set_smob_mark(guile::recombination_node_tag, mark_recombination_node);
    scm_set_smob_free(guile::recombination_node_tag, free_recombination_node);

    guile::gene_conversion_node_tag
	= scm_make_smob_type("gene_conversion-node", 
			     sizeof(GeneConversionNode));
    scm_set_smob_mark(guile::gene_conversion_node_tag,
		      mark_gene_conversion_node);
    scm_set_smob_free(guile::gene_conversion_node_tag,
		      free_gene_conversion_node);


    scm_c_define_gsubr("event-time", 1, 0, 0, 
		       (scm_unused_struct*(*)())event_time);
    scm_c_define_gsubr("recombination-point", 1, 0, 0, 
		       (scm_unused_struct*(*)())recombination_point);
    scm_c_define_gsubr("gene-conversion-start", 1, 0, 0, 
		       (scm_unused_struct*(*)())gene_conversion_start);
    scm_c_define_gsubr("gene-conversion-end", 1, 0, 0, 
		       (scm_unused_struct*(*)())gene_conversion_end);

}
