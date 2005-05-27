/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#ifndef GUILE__NODES_HH_INCLUDED
#define GUILE__NODES_HH_INCLUDED


#ifndef LIBGUILE_H_INCLUDED
# include <libguile.h>
# define LIBGUILE_H_INCLUDED
#endif

namespace core {
    class Node;
    class LeafNode;
    class CoalescentNode;
    class RecombinationNode;
    class GeneConversionNode;
}

namespace guile {

    void install_nodes();
    extern scm_t_bits leaf_node_tag;
    extern scm_t_bits coalescent_node_tag;
    extern scm_t_bits recombination_node_tag;
    extern scm_t_bits gene_conversion_node_tag;

    SCM wrap_leaf_node            (SCM arg, const core::LeafNode *node);
    SCM wrap_coalescent_node      (SCM arg, const core::CoalescentNode *node);
    SCM wrap_recombination_node   (SCM arg, 
				   const core::RecombinationNode *node);
    SCM wrap_gene_conversion_node (SCM arg, 
				   const core::GeneConversionNode *node);

    SCM wrap_node                 (SCM arg, const core::Node *node);
    const core::Node *unwrap_node (SCM node_smob);
}

#endif
