/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005, 2006 by Bioinformatics ApS
 *                                    and Thomas Mailund <mailund@mailund.dk>
 */

#include "nodes.hh"

#ifndef GUILE__SIMULATE_HH_INCLUDED
# include "simulate.hh"
#endif

#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
#endif


scm_t_bits guile::leaf_node_tag;
scm_t_bits guile::coalescent_node_tag;
scm_t_bits guile::recombination_node_tag;
scm_t_bits guile::gene_conversion_node_tag;

using namespace guile;

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

SCM
guile::wrap_node(SCM arg, const core::Node *node)
{
    if (const core::LeafNode *lnode
	= dynamic_cast<const core::LeafNode*>(node))
	return wrap_leaf_node(arg, lnode);
    else if (const core::CoalescentNode *cnode
	     = dynamic_cast<const core::CoalescentNode*>(node))
	return wrap_coalescent_node(arg, cnode);
    else if (const core::RecombinationNode *rnode
	     = dynamic_cast<const core::RecombinationNode*>(node))
	return wrap_recombination_node(arg, rnode);
    else if (const core::GeneConversionNode *gnode
	     = dynamic_cast<const core::GeneConversionNode*>(node))
	return wrap_gene_conversion_node(arg, gnode);
    assert(false); // shouldn't be reached
    return SCM_BOOL_F;
}

const core::Node *
guile::unwrap_node(SCM node_smob)
{
    const core::Node *core_node;
    if (SCM_SMOB_PREDICATE(guile::leaf_node_tag, node_smob))
	{
	    LeafNode *lnode = (LeafNode*)SCM_SMOB_DATA(node_smob);
	    core_node = lnode->node;
	}
    else if (SCM_SMOB_PREDICATE(guile::coalescent_node_tag, node_smob))
	{
	    CoalescentNode *cnode 
		= (CoalescentNode*)SCM_SMOB_DATA(node_smob);
	    core_node = cnode->node;
	}
    else if (SCM_SMOB_PREDICATE(guile::recombination_node_tag, node_smob))
	{
	    RecombinationNode *rnode 
		= (RecombinationNode*)SCM_SMOB_DATA(node_smob);
	    core_node = rnode->node;
	}
    else if (SCM_SMOB_PREDICATE(guile::gene_conversion_node_tag, node_smob))
	{
	    GeneConversionNode *gnode 
		= (GeneConversionNode*)SCM_SMOB_DATA(node_smob);
	    core_node = gnode->node;
	}

    if (!core_node)
	scm_throw(scm_str2symbol("not-a-node-error"), SCM_EOL);

    return core_node;
}

namespace {

    

/* --<GUILE COMMENT>--------------------------------------------- 
<method name="ancestral?">
  <brief>Checks if a node contains ancestral material in a given point.</brief>
  <prototype>(ancestral? node point)</prototype>
  <example>(define coa-times '()) ; times for coalescent events at point 0.5
(define (coa-cb n k) 
  (if (ancestral? n 0.5) (set! coa-times (cons (event-time n) coa-times))))
(simulate markers no-leaves :rho 400 :coalescence-callback coa-cb) </example>
  <description>
    <p>Checks if a node contains ancestral material in a given point.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    bool c_ancestral_p(const core::Node *node, double point)
    {
	return node->contains_point(point);
    }
    SCM ancestral_p(SCM node_smob, SCM s_point)
    {
	double point  = scm_num2dbl(s_point,  "ancestral?");
	const core::Node *node = unwrap_node(node_smob);
	return SCM_BOOL(c_ancestral_p(node, point));
    }

/* --<GUILE COMMENT>--------------------------------------------- 
<method name="trapped?">
  <brief>Checks if a node contains trapped material in a given point.</brief>
  <prototype>(trapped? node point)</prototype>
  <example>(define coa-times '()) ; times for coalescent events at point 0.5
(define (coa-cb n k) 
  (if (or (ancestral? n 0.5) (trapped? n 0.5))(set! coa-times (cons (event-time n) coa-times))))
(simulate markers no-leaves :rho 400 :coalescence-callback coa-cb) </example>
  <description>
    <p>Checks if a node contains trapped material in a given point.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM trapped_p(SCM node_smob, SCM s_point)
    {
	double point  = scm_num2dbl(s_point,  "trapped?");
	const core::Node *node = unwrap_node(node_smob);

	if (c_ancestral_p(node, point))
	    return SCM_BOOL(false); // it is not trapped if it is ancestral

	// it is trapped if it isn't outside the nodes start and stop
	// ancestral point
	bool trapped = 
	    (node->intervals().first_point() <= point) and
	    (point < node->intervals().last_point());
	
	return SCM_BOOL(trapped);
    }




/* --<GUILE COMMENT>---------------------------------------------

<method name="children">
  <brief>Returns the children of a node.</brief>
  <prototype>(children node)</prototype>
  <example>(define (collect-event-times i)
  (letrec ((collect-et-r
	    (lambda (root point)
	      (if (not (ancestral? root point))
		  '()
		  (cond ((leaf-node? root) 
			 (list (event-time root)))
			((coalescent-node? root)
			 (let* ((cs (children root))
				(left (collect-et-r (car cs) point))
				(right (collect-et-r (cadr cs) point)))
			   (cons (event-time root) (append left right))))
			((or (recombination-node? root) 
			     (gene-conversion-node? root))
			 (cons (event-time root)
			       (collect-et-r (car (children root))
						point))))))))
    (collect-et-r (root i) (interval-start i))))</example>
  <description>
    <p>Returns the children of a node.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM children(SCM node_smob)
    {
	if (SCM_SMOB_PREDICATE(guile::leaf_node_tag, node_smob))
	    {
		return SCM_EOL;
	    }
	else if (SCM_SMOB_PREDICATE(guile::coalescent_node_tag, node_smob))
	    {
		CoalescentNode *cnode 
		    = (CoalescentNode*)SCM_SMOB_DATA(node_smob);
		SCM left  = wrap_node(cnode->arg, cnode->node->left_child());
		SCM right = wrap_node(cnode->arg, cnode->node->right_child());
		return scm_list_2(left, right);
	    }
	else if (SCM_SMOB_PREDICATE(guile::recombination_node_tag, node_smob))
	    {
		RecombinationNode *rnode 
		    = (RecombinationNode*)SCM_SMOB_DATA(node_smob);
		return scm_list_1(wrap_node(rnode->arg, rnode->node->child()));

	    }
	else if (SCM_SMOB_PREDICATE(guile::gene_conversion_node_tag, node_smob))
	    {
		GeneConversionNode *gnode 
		    = (GeneConversionNode*)SCM_SMOB_DATA(node_smob);
		return scm_list_1(wrap_node(gnode->arg, gnode->node->child()));
	    }

	scm_throw(scm_str2symbol("not-a-node-error"), SCM_EOL);
	return SCM_EOL; // never reached
    }


    
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

<method name="leaf-node?">
  <brief>A predicate recognising leaf nodes.</brief>
  <prototype>(leaf-node? node)</prototype>
  <example>(if (leaf-node? n) 0.0 (event-time n))</example>
  <description>
    <p>Returns #t if `node` is a leaf node, #f otherwise.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM leaf_node_p(SCM smob)
    {
	return SCM_BOOL(SCM_SMOB_PREDICATE(leaf_node_tag, smob));
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="coalescent-node?">
  <brief>A predicate recognising coalescent nodes.</brief>
  <prototype>(coalescent-node? node)</prototype>
  <example>(if (coalescent-node? n) (event-time n))</example>
  <description>
    <p>Returns #t if `node` is a coalescent node, #f otherwise.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM coalescent_node_p(SCM smob)
    {
	return SCM_BOOL(SCM_SMOB_PREDICATE(coalescent_node_tag, smob));
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="recombination-node?">
  <brief>A predicate recognising recombination nodes.</brief>
  <prototype>(recombination-node? node)</prototype>
  <example>(if (recombination-node? n) (event-time n))</example>
  <description>
    <p>Returns #t if `node` is a recombination node, #f otherwise.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM recombination_node_p(SCM smob)
    {
	return SCM_BOOL(SCM_SMOB_PREDICATE(recombination_node_tag, smob));
    }

/* --<GUILE COMMENT>---------------------------------------------

<method name="gene-conversion-node?">
  <brief>A predicate recognising gene conversion nodes.</brief>
  <prototype>(gene-conversion-node? node)</prototype>
  <example>(if (gene-conversion-node? n) (event-time n))</example>
  <description>
    <p>Returns #t if `node` is a gene conversion node, #f otherwise.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    SCM gene_conversion_node_p(SCM smob)
    {
	return SCM_BOOL(SCM_SMOB_PREDICATE(gene_conversion_node_tag, smob));
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



/* --<GUILE COMMENT>---------------------------------------------

<method name="fold-nodes">
  <brief>Calculate a value from all ARG nodes.</brief>
  <prototype>(fold-nodes arg f init)</prototype>
  <example>(define no-nodes
  (fold-nodes arg (lambda (n count) (+ count 1)) 0))
(define no-leaves
  (fold-nodes arg (lambda (n count) (if (leaf-node? n) (+ count 1) count)) 0))</example>
  <description>
    <p>
      Calculates a value from all ARG nodes.  Call function `f' on each node
      in the ARG, accumulating a value that is the result of the fold.  For
      each node, `f' is called with the node as its first argument and the
      value calculated so far as its second argument; the second argument is
      initially `init'.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
    SCM fold_nodes(SCM s_arg, SCM f, SCM init)
    {
	SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, s_arg),
		   s_arg, SCM_ARG1, "fold-nodes");
	SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(f)), f, SCM_ARG2, "fold-nodes");

	ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(s_arg);
	const core::ARG *arg = arg_data->arg;
	const std::vector<core::Node*> &leaves = arg->leaves();
	const std::vector<core::Node*> &inner = arg->inner_nodes();

	SCM val = init;
	std::vector<core::Node*>::const_iterator i;
	for (i = leaves.begin(); i != leaves.end(); ++i)
	    val = scm_apply_2(f, wrap_node(s_arg, *i), val, SCM_EOL);
	for (i = inner.begin(); i != inner.end(); ++i)
	    val = scm_apply_2(f, wrap_node(s_arg, *i), val, SCM_EOL);

	return val;
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


    scm_c_define_gsubr("leaf-node?", 1, 0, 0, 
		       (scm_unused_struct*(*)())leaf_node_p);
    scm_c_define_gsubr("coalescent-node?", 1, 0, 0, 
		       (scm_unused_struct*(*)())coalescent_node_p);
    scm_c_define_gsubr("recombination-node?", 1, 0, 0, 
		       (scm_unused_struct*(*)())recombination_node_p);
    scm_c_define_gsubr("gene-conversion-node?", 1, 0, 0, 
		       (scm_unused_struct*(*)())gene_conversion_node_p);

    scm_c_define_gsubr("event-time", 1, 0, 0, 
		       (scm_unused_struct*(*)())event_time);
    scm_c_define_gsubr("ancestral?", 2, 0, 0, 
		       (scm_unused_struct*(*)())ancestral_p);
    scm_c_define_gsubr("trapped?", 2, 0, 0, 
		       (scm_unused_struct*(*)())trapped_p);
    scm_c_define_gsubr("children", 1, 0, 0, 
		       (scm_unused_struct*(*)())children);

    scm_c_define_gsubr("recombination-point", 1, 0, 0, 
		       (scm_unused_struct*(*)())recombination_point);

    scm_c_define_gsubr("gene-conversion-start", 1, 0, 0, 
		       (scm_unused_struct*(*)())gene_conversion_start);
    scm_c_define_gsubr("gene-conversion-end", 1, 0, 0, 
		       (scm_unused_struct*(*)())gene_conversion_end);

    scm_c_define_gsubr("fold-nodes", 3, 0, 0, 
		       (scm_unused_struct*(*)())fold_nodes);

}
