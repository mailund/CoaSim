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
    class CLISimMonitor : public core::SimulationMonitor {
	void start_arg_building(unsigned int no_leaves);
	void builder_update(unsigned int no_nodes, unsigned int no_top_nodes,
			    unsigned long int no_iterations, double cur_time,
			    unsigned int no_coal_events,
			    unsigned int no_gene_conv_events,
			    unsigned int no_recomb_events);
	void builder_termination(unsigned int no_nodes,
				 unsigned int no_top_nodes,
				 unsigned long int no_iterations,
				 double cur_time,
				 unsigned int no_coal_events,
				 unsigned int no_gene_conv_events,
				 unsigned int no_recomb_events);

	void start_mutating();
	void mutator_update(unsigned int marker_no);
	void retry_mutation();
	void retry_arg_building();

	void simulation_terminated();
    };
}


namespace {
    size_t free_arg(SCM s_arg_data)
    {
	ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(s_arg_data);
	arg_data->~ARGData();
	scm_must_free(arg_data);
	return sizeof(ARGData);
    }
}




void CLISimMonitor::start_arg_building(unsigned int no_leaves)
{
    std::cout << "START BUILDING ARG...\n";
}

void CLISimMonitor::builder_update(unsigned int no_nodes,
				   unsigned int no_top_nodes,
				   unsigned long int no_iterations, 
				   double cur_time,
				   unsigned int no_coal_events,
				   unsigned int no_gene_conv_events,
				   unsigned int no_recomb_events)
{
    std::cout << "Iteration: " << no_iterations
	      << " time " << cur_time << '\n'
	      << no_nodes << " nodes in ARG, "
	      << no_top_nodes << " remaining to be processed.\n"
	      << '\t' << no_coal_events << " coalescence events\n"
	      << '\t' << no_gene_conv_events << " gene conversion events\n"
	      << '\t' << no_recomb_events << " recombination events\n";
}

void CLISimMonitor::builder_termination(unsigned int no_nodes,
					unsigned int no_top_nodes,
					unsigned long int no_iterations,
					double cur_time,
					unsigned int no_coal_events,
					unsigned int no_gene_conv_events,
					unsigned int no_recomb_events)
{
    std::cout << "\nARG Building terminated after " << no_iterations 
	      << " iterations at time " << cur_time << '\n'
	      << no_nodes << " nodes in ARG, "
	      << no_top_nodes << " remaining to be processed.\n"
	      << '\t' << no_coal_events << " coalescence events\n"
	      << '\t' << no_gene_conv_events << " gene conversion events\n"
	      << '\t' << no_recomb_events << " recombination events\n\n";
}

void CLISimMonitor::start_mutating()
{
    std::cout << "START MUTATING ARG...\n";
}

void CLISimMonitor::mutator_update(unsigned int marker_no)
{
    std::cout << "Mutating marker " << marker_no << "...\n";
}

void CLISimMonitor::retry_mutation()
{
    std::cout << "\tmutation not withing bounds, retrying...\n";
}

void CLISimMonitor::retry_arg_building()
{
    std::cout << "\tmutation not withing bounds of trait marker\n"
	      << "\tbuilding new ARG...\n\n";
}

void CLISimMonitor::simulation_terminated()
{
    std::cout << "\nSIMULATION COMPLETED\n";
}

void ProfileMonitor::builder_termination(unsigned int no_nodes,
					 unsigned int no_top_nodes,
					 unsigned long int no_iterations,
					 double cur_time,
					 unsigned int no_coal_events,
					 unsigned int no_gene_conv_events,
					 unsigned int no_recomb_events)
{
    this->no_nodes = no_nodes;
    this->no_top_nodes = no_top_nodes;
    this->no_iterations = no_iterations;
    this->termination_time = cur_time;
    this->no_coal_events = no_coal_events;
    this->no_gene_conv_events = no_gene_conv_events;
    this->no_recomb_events = no_recomb_events;
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
	 SCM s_no_leaves,	// 2
	 SCM s_sim_parameters,	// 3
	 SCM coa_cb, 		// 4
	 SCM rc_cb, 		// 5
	 SCM gc_cb, 		// 6
	 SCM s_keep_empty,	// 7
	 SCM s_random_seed)	// 8
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

    int no_leaves = scm_num2int(s_no_leaves, SCM_ARG2, "simulate");

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
		       rc_cb, 5, "c-simulate");
	    cb.set_rc_cb(rc_cb);
	    has_cb = true;
	}
    if (gc_cb != SCM_EOL)
	{
	    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(gc_cb)),
		       gc_cb, 6, "c-simulate");
	    cb.set_gc_cb(gc_cb);
	    has_cb = true;
	}

    bool keep_empty = SCM_NFALSEP(s_keep_empty);
    unsigned int seed = scm_num2int(s_random_seed, 8, "c-simulate");

    try {
	auto_ptr<ProfileMonitor> monitor(new ProfileMonitor());
	auto_ptr<core::Configuration> conf(new core::Configuration(no_leaves,
								   markers.begin(), markers.end(),
								   rho,
								   Q, gamma, 
								   beta));
	auto_ptr<core::ARG> arg(core::Simulator::simulate(*conf, 
							  monitor.get(),
							  has_cb ? &cb : 0,
							  keep_empty,
							  seed));

	void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
	ARGData *arg_data = new(mem)ARGData(arg.release(),conf.release(),monitor.release());
    
	SCM_RETURN_NEWSMOB(guile::arg_tag, arg_data);
    } catch(core::Configuration::out_of_sequence&) {
	scm_throw(scm_str2symbol("out-of-sequence"), s_markers);
    } catch(SchemeException &sex) { // ;-)
	propagate(sex);
    } catch(exception &ex) {
	scm_throw(scm_str2symbol("unexcepted-exception"), 
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




/* --<GUILE COMMENT>---------------------------------------------

<method name="no-recombinations">
  <brief>Returns the number of recombinations in the ARG.</brief>
  <prototype>(no-recombinations arg)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate markers 100 :rho 400))
(define n (no-recombinations arg))</example>
  <description>
    <p>
     Returns the number of recombinations in the ARG.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM 
no_recomb_events(SCM arg_data_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "no-recombinations");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);

    return scm_int2num(arg_data->monitor->no_recomb_events);
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="no-coalescence-events">
  <brief>Returns the number of coalescence events in the ARG.</brief>
  <prototype>(no-coalescence-events arg)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate markers 100 :rho 400))
(define n (no-coalescence-events arg))</example>
  <description>
    <p>
     Returns the number of coalescence events in the ARG.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM 
no_coal_events(SCM arg_data_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "no-coalescence-events");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);

    return scm_int2num(arg_data->monitor->no_coal_events);
}


/* --<GUILE COMMENT>---------------------------------------------

<method name="no-gene-conversions">
  <brief>Returns the number of gene conversions in the ARG.</brief>
  <prototype>(no-gene-conversions arg)</prototype>
  <example>(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate markers 100 :gamma 10 :Q 0.2))
(define n (no-gene-conversions arg))</example>
  <description>
    <p>
     Returns the number of gene conversions in the ARG.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM 
no_gene_conv_events(SCM arg_data_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "no-gene-conversions");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);

    return scm_int2num(arg_data->monitor->no_gene_conv_events);
}





void
guile::install_simulate()
{
    guile::arg_tag = scm_make_smob_type("arg", sizeof(ARGData));
    scm_set_smob_free(guile::arg_tag, free_arg);

    scm_c_define_gsubr("c-simulate", 8, 0, 0, 
		       (scm_unused_struct*(*)())simulate);
    scm_c_eval_string("(use-modules (ice-9 optargs))"
		      "(define (simulate ms n . args)"
		      "  (let-keywords args #f ((rho   0)"
		      "                         (gamma 0)"
		      "                         (Q     0)"
		      "                         (beta  0)"
		      "                         (coalescence-callback '())"
		      "                         (recombination-callback '())"
		      "                         (geneconversion-callback '())"
		      "                         (keep-empty-intervals #f)"
		      "                         (random-seed 0))"
		      "		(c-simulate ms n"
		      "                     (list rho gamma Q beta)"
		      "                     coalescence-callback"
		      "                     recombination-callback"
		      "                     geneconversion-callback"
		      "                     keep-empty-intervals"
		      "                     random-seed)))");


    scm_c_define_gsubr("sequences", 1, 0, 0, 
		       (scm_unused_struct*(*)())sequences);

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

    scm_c_define_gsubr("intervals", 1, 0, 0, 
		       (scm_unused_struct*(*)())intervals);
    scm_c_eval_string("(define (local-trees arg)"
		      "  (map interval->tree (intervals arg)))");

    scm_c_define_gsubr("no-recombinations", 1, 0, 0, 
		       (scm_unused_struct*(*)())no_recomb_events);
    scm_c_define_gsubr("no-coalescence-events", 1, 0, 0, 
		       (scm_unused_struct*(*)())no_coal_events);
    scm_c_define_gsubr("no-gene-conversions", 1, 0, 0, 
		       (scm_unused_struct*(*)())no_gene_conv_events);
}

