/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004, 2005 by Bioinformatics ApS
 */

#include "simulate.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__EPOCHS_HH_INCLUDED
# include "epochs.hh"
#endif
#ifndef GUILE__OPTIONS_HH_INCLUDED
# include "options.hh"
#endif
#ifndef GUILE__NODES_HH_INCLUDED
# include "nodes.hh"
#endif
#ifndef GUILE__INTERVALS_HH_INCLUDED
# include "intervals.hh"
#endif
#ifndef GUILE__EXCEPTIONS_HH_INCLUDED
# include "exceptions.hh"
#endif

using namespace guile;


#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif
#ifndef CORE__EPOCHS_HH_INCLUDED
# include <Core/epochs.hh>
#endif


#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_H_INCLUDED
#endif
#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif


scm_t_bits guile::arg_tag;

namespace {
    size_t free_arg(SCM s_arg_data)
    {
	ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(s_arg_data);
	arg_data->~ARGData();
	scm_must_free(arg_data);
	return sizeof(ARGData);
    }
}





/* --<GUILE COMMENT>---------------------------------------------

<method name="simulate">
  <brief>Simulate an ARG and corresponding sequences.</brief>
  <prototype>(simulate marker-list no-leaves . additional-keyword-parameters)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate markers 100 :rho 400 :beta 10))

(define coa-times '())
(define (coa-cb n k) (set! coa-times (cons (event-time n) coa-times)))
(simulate markers 10 :coalescence-callback coa-cb :rho 400 :beta 10)
(display "coalescence times:\n")
(map (lambda (t) (display t)(newline)) coa-times)
(newline)</example>
  <description>
    <p>
      Simulate an ARG and corresponding sequences, based ARG parameters, 
      a list of markers, and the number of markers to simulate.
    </p>
    <p>
      The building of the ARG is affected by the following paramters, that 
      can be set using keyword arguments:
    </p>
    <ul>
      <li><b>rho:</b> the <em>scaled recombination rate</em>, rho=4Nr.  See 
        e.g. <em>Hein, Schierup and Wiuf:</em> Gene Genealogies, Variation
        and Evolution, section 5.5 for details.
	By default, this parameter is 0.
      </li>
      <li><b>gamma:</b> the <em>scaled gene-conversion rate</em>, gamma=4Ng.
        See e.g. <em>Hein, et al.:</em> section 5.10 for details.
	By default, this parameter is 0.
      </li>
      <li><b>Q:</b> the <em>gene-conversion tract length intensity</em>.
        See e.g. <em>Hein, et al.:</em> section 5.10 for details.
	By default, this parameter is 0.
      </li>
      <li><b>beta:</b> the <em>exponential growth rate</em>, beta=2Nb.
        See e.g. <em>Hein, et al.:</em> section 4.3 for details.
	By default, this parameter is 0.
      </li>
    </ul>
    <p>
      For fine-monitoring of the simulation, callback functions can be given
      as key-word arguments.  The supported callbacks are:
    </p>
    <ul>
      <li><b>coalescence-callback:</b>
        called with the single node that is the result of a coalescent event,
        and the number of lineages at the time of the coalescent (i.e.
        the number of linages just after the event, moving forward in time).
      </li>
      <li><b>recombination-callback:</b>
          called with the two nodes that is the result of a recombination
          event, and the number of lineages at the time of the recombination
	  (i.e. the number of linages just after the event, moving forward 
	  in time).
      </li>
      <li><b>geneconversion-callback:</b>
          called with the two nodes that is the result of a gene conversion
          event, and the number of lineages at the time of the gene conversion
	  (i.e. the number of linages just after the event, moving forward 
	  in time).
      </li>
    </ul>
    <p>
      The following additional keyword arguments are available:
    </p>
    <ul>
      <li><b>keep-empty-intervals:</b> Turns off an optimisation that 
          removes intervals not containing markers from the ARG simulation.
          This option is useful when the simulation is concerned with ARG
          properties rather than just the resulting sequences.
      </li>
      <li><b>random-seed:</b> an integer used as random seed for the
          simulation.  In most cases, this argument will not be used, but
          it is useful for regression testing of scheme modules for CoaSim.
      </li>
    </ul>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

class Callbacks : public core::BuilderMonitor
{
    SCM i_coa_cb;
    SCM i_rc_cb;
    SCM i_gc_cb;

    bool i_has_coa_cb;
    bool i_has_rc_cb;
    bool i_has_gc_cb;

public:
    Callbacks() : i_has_coa_cb(false),
		  i_has_rc_cb(false), 
		  i_has_gc_cb(false)
    {};

    void set_coa_cb(SCM cb)
    {
	i_coa_cb = cb;
	i_has_coa_cb = true;
    }
    void set_rc_cb(SCM cb)
    {
	i_rc_cb = cb;
	i_has_rc_cb = true;
    }
    void set_gc_cb(SCM cb)
    {
	i_gc_cb = cb;
	i_has_gc_cb = true;
    }

    virtual void coalescence_callback(core::CoalescentNode *n, int k);
    virtual void recombination_callback(core::RecombinationNode *n1,
					core::RecombinationNode *n2,
					int k);
    virtual void gene_conversion_callback(core::GeneConversionNode *n1,
					  core::GeneConversionNode *n2,
					  int k);
};


void Callbacks::coalescence_callback(core::CoalescentNode *n, int k)
{
    if (!i_has_coa_cb) return;
    // fake ARG -- real does not exist yet
    SCM node = wrap_coalescent_node(SCM_EOL, n);
    SCM s_k   = scm_int2num(k);
    wrapped_apply(i_coa_cb, scm_list_2(node, s_k));
}

void Callbacks::recombination_callback(core::RecombinationNode *n1,
				       core::RecombinationNode *n2,
				       int k)
{
    if (!i_has_rc_cb) return;
    // fake ARG -- real does not exist yet
    SCM node1 = wrap_recombination_node(SCM_EOL, n1);
    SCM node2 = wrap_recombination_node(SCM_EOL, n2);
    SCM s_k   = scm_int2num(k);
    wrapped_apply(i_rc_cb, scm_list_3(node1, node2, s_k));
}

void Callbacks::gene_conversion_callback(core::GeneConversionNode *n1,
					 core::GeneConversionNode *n2,
					 int k)
{
    if (!i_has_gc_cb) return;
    // fake ARG -- real does not exist yet
    SCM node1 = wrap_gene_conversion_node(SCM_EOL, n1);
    SCM node2 = wrap_gene_conversion_node(SCM_EOL, n2);
    SCM s_k   = scm_int2num(k);
    wrapped_apply(i_gc_cb, scm_list_3(node1, node2, s_k));
}

static SCM
simulate(SCM s_markers,		// 1
	 SCM s_pop_size,	// 2
	 SCM s_sim_parameters,	// 3
	 SCM s_callbacks,       // 4
	 SCM s_epochs,          // 5
	 SCM s_keep_empty,	// 6
	 SCM s_random_seed)	// 7
{
    using namespace std;

    SCM_ASSERT(SCM_NFALSEP(scm_list_p(s_markers)),
	       s_markers, SCM_ARG1, "simulate");

    SCM s_rho   = SCM_CAR(s_sim_parameters);
    SCM s_gamma = SCM_CADR(s_sim_parameters);
    SCM s_Q     = SCM_CADDR(s_sim_parameters);
    SCM s_beta  = SCM_CADDDR(s_sim_parameters);

    double rho   = scm_num2dbl(s_rho,   "simulate");
    double Q     = scm_num2dbl(s_Q,     "simulate");
    double gamma = scm_num2dbl(s_gamma, "simulate");
    double beta  = scm_num2dbl(s_beta,  "simulate");

    SCM itr_markers = s_markers;
    vector<core::Marker*> markers;
    while (!SCM_NULLP(itr_markers))
	{
	    SCM marker_smob = SCM_CAR(itr_markers);
	    assert_marker(marker_smob, SCM_ARG1, "simulate");

	    core::Marker *marker = (core::Marker*) SCM_SMOB_DATA(marker_smob);
	    markers.push_back(marker);

	    itr_markers = SCM_CDR(itr_markers);
	}

    SCM itr_epochs = s_epochs;
    vector<core::Epoch*> epochs;
    while (!SCM_NULLP(itr_epochs))
	{
	    SCM epoch_smob = SCM_CAR(itr_epochs);
	    assert_epoch(epoch_smob, 5, "simulate");

	    core::Epoch *epoch = (core::Epoch*) SCM_SMOB_DATA(epoch_smob);
	    epochs.push_back(epoch);

	    itr_epochs = SCM_CDR(itr_epochs);
	}

    SCM_ASSERT(SCM_NFALSEP(scm_list_p(s_pop_size)), s_pop_size, 2, "simulate");
    std::vector<int> pop_sizes;

    SCM itr_pop_size = s_pop_size;
    while (!SCM_NULLP(itr_pop_size))
	{
	    SCM s_pop = SCM_CAR(itr_pop_size);
	    int pop = scm_num2int(s_pop, 2, "simulate");
	    pop_sizes.push_back(pop);
	    itr_pop_size = SCM_CDR(itr_pop_size);
	}

    SCM_ASSERT(SCM_NFALSEP(scm_list_p(s_callbacks)),
	       s_callbacks, 4, "simulate");

    SCM coa_cb = SCM_CAR(s_callbacks);
    SCM rc_cb  = SCM_CADR(s_callbacks);
    SCM gc_cb  = SCM_CADDR(s_callbacks);

    bool has_cb = false;
    Callbacks cb;
    if (coa_cb != SCM_EOL)
	{
	    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(coa_cb)),
		       coa_cb, SCM_ARG4, "c-simulate");
	    cb.set_coa_cb(coa_cb);
	    has_cb = true;
	}
    if (rc_cb != SCM_EOL)
	{
	    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(rc_cb)),
		       rc_cb, 4, "c-simulate");
	    cb.set_rc_cb(rc_cb);
	    has_cb = true;
	}
    if (gc_cb != SCM_EOL)
	{
	    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(gc_cb)),
		       gc_cb, 4, "c-simulate");
	    cb.set_gc_cb(gc_cb);
	    has_cb = true;
	}

    bool keep_empty = SCM_NFALSEP(s_keep_empty);
    unsigned int seed = scm_num2int(s_random_seed, 7, "c-simulate");

    try {
	using core::Configuration;
	using core::ARG;
	
	auto_ptr<Configuration> conf(new Configuration(pop_sizes.begin(),
						       pop_sizes.end(),
						       markers.begin(),
						       markers.end(),
						       epochs.begin(),
						       epochs.end(),
						       rho,
						       Q, gamma, 
						       beta));
	auto_ptr<ARG> arg(core::Simulator::simulate(*conf, 
						    has_cb ? &cb : 0,
						    keep_empty,
						    seed));

	void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
	ARGData *arg_data = new(mem)ARGData(arg.release(),
					    conf.release());
    
	SCM_RETURN_NEWSMOB(guile::arg_tag, arg_data);
    } catch(core::Configuration::out_of_sequence&) {
	scm_throw(scm_str2symbol("out-of-sequence"), s_markers);
    } catch(core::Configuration::non_pos_pop_size&) {
	scm_throw(scm_str2symbol("non-positive-sample-size"), s_pop_size);
    } catch(SchemeException &sex) { // ;-)
	propagate(sex);
    } catch(exception &ex) {
	scm_throw(scm_str2symbol("unexpected-exception"), 
		  scm_mem2string(ex.what(),strlen(ex.what())));
    }

    // shouldn't really reach...
    return SCM_EOL;
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="sequences">
  <brief>Returns the simulated sequences of an ARG as a list of lists.</brief>
  <prototype>(sequences arg)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define haplotypes (let ((arg (simulate markers 100 :rho 400))) (sequences arg)))</example>
  <description>
    <p>Returns the simulated sequences of an ARG as a list of lists.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM 
sequences(SCM arg_data_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "sequences");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);

    SCM sequences = SCM_EOL;

    const std::vector<core::Node*> &leaves = arg_data->arg->leaves();
    std::vector<core::Node*>::const_iterator i;
    for (i = leaves.begin(); i != leaves.end(); ++i)
	{
	    SCM seq = SCM_EOL;
	    for (size_t j = 0; j < (*i)->no_states(); ++j)
		seq = scm_cons(scm_int2num((*i)->state(j)), seq);
	    sequences = scm_cons(scm_reverse(seq), sequences);
	}

    return scm_reverse(sequences);
}


// FIXME: I want a better example here!!!
/* --<GUILE COMMENT>---------------------------------------------

<method name="intervals">
  <brief>Returns the intervals sharing genealogy in the ARG as a list.</brief>
  <prototype>(intervals arg)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate markers 100 :rho 400))) (intervals arg)))</example>
  <description>
    <p>
     Returns the intervals sharing genealogy in the ARG as a list.
     Only intervals containing markers are returned.
    </p>
  </description>
</method>

<method name="local-trees">
  <brief>Returns the local trees, i.e. the trees for intervals sharing genealogy in the ARG as a list.</brief>
  <prototype>(local-trees arg)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define trees (let ((arg (simulate markers 100 :rho 400))) (local-trees arg)))</example>
  <description>
    <p>
     Returns the local trees, i.e. the trees for 
     intervals sharing genealogy in the ARG as a list.
     Only trees containing markers are returned.
    </p>
  </description>
</method>


-----</GUILE COMMENT>-------------------------------------------- */
static SCM 
intervals(SCM arg_data_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "intervals");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);

    SCM intervals = SCM_EOL;

    const std::vector<core::RetiredInterval> &rintervals
	= arg_data->arg->retired_intervals();
    std::vector<core::RetiredInterval>::const_iterator i;
    for (i = rintervals.begin(); i != rintervals.end(); ++i)
	{
	    SCM interval = wrap_interval(arg_data_smob, &(*i));
	    intervals = scm_cons(interval, intervals);
	}

    return scm_reverse(intervals);
}


// include scheme source
#include "simulate.scm.include"

void
guile::install_simulate()
{
    guile::arg_tag = scm_make_smob_type("arg", sizeof(ARGData));
    scm_set_smob_free(guile::arg_tag, free_arg);

    scm_c_define_gsubr("c-simulate", 7, 0, 0, 
		       (scm_unused_struct*(*)())simulate);
    scm_c_eval_string(SIMULATE_SCM_INCLUDE);

    scm_c_define_gsubr("sequences", 1, 0, 0, 
		       (scm_unused_struct*(*)())sequences);
    scm_c_define_gsubr("intervals", 1, 0, 0, 
		       (scm_unused_struct*(*)())intervals);
    scm_c_eval_string("(define (local-trees arg)"
		      "  (map interval->tree (intervals arg)))");

/* --<GUILE COMMENT>---------------------------------------------
       
<method name="simulate-sequences">
  <brief>Simulate a list of sequences.</brief>
  <prototype>(simulate-sequences markers no-leaves . additional-simulation-parameters)</prototype>
  <example>(define markers (make-random-snp-markers 10 0 1))
(define seqs (simulate-sequences markers 10 :rho 400))</example>
  <description>
    <p>
     Simulate a list of sequences.  This function is just a short-cut
     for `simulate' followed by `sequences': 
    </p>
    <code><pre> (define (simulate-sequences . args)
   (sequences (apply simulate args)))</pre></code>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */

    scm_c_eval_string("(define (simulate-sequences . params)"
		      "  (sequences (apply simulate params)))");

}

