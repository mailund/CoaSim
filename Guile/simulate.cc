/* -*- Mode: C++; c-basic-offset: 4; -*- 
 *
 *  CoaSim -- A coalescence process simulator
 *
 *  Copyright (C) 2004 by Bioinformatics ApS
 */

#include "simulate.hh"

#ifndef GUILE__MARKER_HH_INCLUDED
# include "marker.hh"
#endif
#ifndef GUILE__ARG_PARAMETERS_HH_INCLUDED
# include "arg_parameters.hh"
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

using namespace guile;


#ifndef CORE__ALL_MARKERS_HH_INCLUDED
# include <Core/all_markers.hh>
#endif
#ifndef CORE__CONFIGURATION_HH_INCLUDED
# include <Core/configuration.hh>
#endif
#ifndef CORE__SIMULATOR_HH_INCLUDED
# include <Core/simulator.hh>
#endif
#ifndef CORE__BUILDER_HH_INCLUDED
# include <Core/builder.hh>
#endif
#ifndef CORE__MONITOR_HH_INCLUDED
# include <Core/monitor.hh>
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


    struct ProfileMonitor : public core::SimulationMonitor {

	unsigned int no_leaves;
	unsigned int no_nodes;
	unsigned int no_top_nodes;
	unsigned int no_iterations;
	double termination_time;
	unsigned int no_coal_events;
	unsigned int no_gene_conv_events;
	unsigned int no_recomb_events;
	
	ProfileMonitor()
	    : no_leaves(0), no_nodes(0), no_top_nodes(0),
	      no_iterations(0), termination_time(0),
	      no_coal_events(0), no_gene_conv_events(0), no_recomb_events(0)
	{}

	void start_arg_building(unsigned int no_leaves) 
	{
	    this->no_leaves = no_leaves;
	}
	void builder_update(unsigned int no_nodes, unsigned int no_top_nodes,
			    unsigned long int no_iterations, double cur_time,
			    unsigned int no_coal_events,
			    unsigned int no_gene_conv_events,
			    unsigned int no_recomb_events) {}
	void builder_termination(unsigned int no_nodes, unsigned int no_top_nodes,
				 unsigned long int no_iterations, double cur_time,
				 unsigned int no_coal_events,
				 unsigned int no_gene_conv_events,
				 unsigned int no_recomb_events);

	void start_mutating() {}
	void mutator_update(unsigned int marker_no) {}
	void retry_mutation() {}
	void retry_arg_building() {}

	void simulation_terminated() {}
    };
}

namespace {
    struct ARGData {
	const core::ARG *arg;
	const core::Configuration *conf;
	const ProfileMonitor *monitor;
	ARGData(core::ARG *arg, core::Configuration *conf,
		ProfileMonitor *monitor)
	    : arg(arg), conf(conf), monitor(monitor)
	{};
	~ARGData()
	{
	    delete arg;
	    delete conf;
	    delete monitor;
	}
    };

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
  <prototype>(simulate arg-parameters marker-list no-leaves . callbacks)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate p markers 100))

(define coa-times '())
(define (coa-cb n) (set! coa-times (cons (event-time n) coa-times)))
(simulate p markers 10 :coalescence-callback coa-cb)
(display "coalescence times:\n")
(map (lambda (t) (display t)(newline)) coa-times)
(newline)</example>
  <description>
    <p>
      Simulate an ARG and corresponding sequences, based ARG parameters, 
      a list of markers, and the number of markers to simulate.
    </p>
    <p>
      For fine-monitoring of the simulation, callback functions can be given
      as key-word arguments.  The supported callbacks are:
    </p>
    <ul>
      <li><b>coalescence-callback:</b>
          called with the single node that is the result of a coalescent event.
      </li>
      <li><b>recombination-callback:</b>
          called with the two nodes that is the result of a recombination
          event.
      </li>
      <li><b>geneconversion-callback:</b>
          called with the two nodes that is the result of a gene conversion
          event.
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

    virtual void coalescence_callback(core::CoalescentNode *n);
    virtual void recombination_callback(core::RecombinationNode *n1,
					core::RecombinationNode *n2);
    virtual void gene_conversion_callback(core::GeneConversionNode *n1,
					  core::GeneConversionNode *n2);
};

void Callbacks::coalescence_callback(core::CoalescentNode *n)
{
    if (!i_has_coa_cb) return;
    // fake ARG -- real does not exist yet
    SCM node = wrap_coalescent_node(SCM_EOL, n);
    scm_apply(i_coa_cb, scm_cons(node, SCM_EOL), SCM_EOL);
}
void Callbacks::recombination_callback(core::RecombinationNode *n1,
				       core::RecombinationNode *n2)
{
    if (!i_has_rc_cb) return;
    // fake ARG -- real does not exist yet
    SCM node1 = wrap_recombination_node(SCM_EOL, n1);
    SCM node2 = wrap_recombination_node(SCM_EOL, n2);
    scm_apply(i_rc_cb, 
	      scm_cons(node1, scm_cons(node2, SCM_EOL)), 
	      SCM_EOL);
}
void Callbacks::gene_conversion_callback(core::GeneConversionNode *n1,
					 core::GeneConversionNode *n2)
{
    if (!i_has_gc_cb) return;
    // fake ARG -- real does not exist yet
    SCM node1 = wrap_gene_conversion_node(SCM_EOL, n1);
    SCM node2 = wrap_gene_conversion_node(SCM_EOL, n2);
    scm_apply(i_gc_cb, 
	      scm_cons(node1, scm_cons(node2, SCM_EOL)), 
	      SCM_EOL);
}

static SCM
simulate(SCM arg_parameters_smob, SCM s_markers, SCM s_no_leaves,
	 SCM coa_cb, SCM rc_cb, SCM gc_cb)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_parameters_tag, 
				  arg_parameters_smob),
	       arg_parameters_smob, SCM_ARG1, "simulate");
    SCM_ASSERT(SCM_NFALSEP(scm_list_p(s_markers)),
	       s_markers, SCM_ARG2, "simulate");

    ARGParameters *p = (ARGParameters*) SCM_SMOB_DATA(arg_parameters_smob);

    SCM itr_markers = s_markers;
    std::vector<core::Marker*> markers;
    while (!SCM_NULLP(itr_markers))
	{
	    SCM marker_smob = SCM_CAR(itr_markers);
	    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::trait_marker_tag, marker_smob)
		       or
		       SCM_SMOB_PREDICATE(guile::snp_marker_tag, marker_smob)
		       or
		       SCM_SMOB_PREDICATE(guile::ms_marker_tag, marker_smob),
		       marker_smob, SCM_ARG2, "simulate");

	    core::Marker *marker = (core::Marker*) SCM_SMOB_DATA(marker_smob);
	    markers.push_back(marker);

	    itr_markers = SCM_CDR(itr_markers);
	}

    int no_leaves = scm_num2int(s_no_leaves, SCM_ARG3, "simulate");

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
		       rc_cb, SCM_ARG5, "c-simulate");
	    cb.set_rc_cb(rc_cb);
	    has_cb = true;
	}
    if (gc_cb != SCM_EOL)
	{
	    SCM_ASSERT(SCM_NFALSEP(scm_procedure_p(gc_cb)),
		       gc_cb, SCM_ARG6, "c-simulate");
	    cb.set_gc_cb(gc_cb);
	    has_cb = true;
	}


    try {
	std::auto_ptr<ProfileMonitor> monitor(new ProfileMonitor());
	std::auto_ptr<core::Configuration> conf(new core::Configuration(no_leaves,
									markers.begin(), markers.end(),
									p->rho, p->Q, p->G, p->growth));
	std::auto_ptr<core::ARG> arg(core::Simulator::simulate(*conf, 
							       monitor.get(),
							       has_cb ? &cb : 0));

	void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
	ARGData *arg_data = new(mem)ARGData(arg.release(),conf.release(),monitor.release());
    
	SCM_RETURN_NEWSMOB(guile::arg_tag, arg_data);
    } catch(core::Configuration::out_of_sequence&) {
	scm_throw(scm_str2symbol("out-of-sequence"), s_markers);
    } catch(std::exception &ex) {
	scm_throw(scm_str2symbol("unexcepted-exception"), 
		  scm_mem2string(ex.what(),strlen(ex.what())));
    }

    // shouldn't really reach...
    return SCM_EOL;
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="save-sequences">
  <brief>Save the sequences from a simulated ARG to a file.</brief>
  <prototype>(save-sequences arg file-name)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate p markers 100))
(save-sequences arg "haplotypes.txt")</example>
  <description>
    <p>Save the sequences from a simulated ARG to a file.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM 
save_sequences(SCM arg_data_smob, SCM s_filename)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::arg_tag, arg_data_smob),
	       arg_data_smob, SCM_ARG1, "save-sequences");
    SCM_ASSERT(SCM_NFALSEP(scm_string_p(s_filename)),
	       s_filename, SCM_ARG2, "save-sequences");

    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(arg_data_smob);
    const char *fname = SCM_STRING_CHARS(s_filename);

    std::ofstream os(fname);
    if (!os) scm_throw(scm_str2symbol("open-error"), s_filename);

    arg_data->arg->to_text(os);
    os.close();

    return SCM_EOL;
}

/* --<GUILE COMMENT>---------------------------------------------

<method name="sequences">
  <brief>Returns the simulated sequences of an ARG as a list of lists.</brief>
  <prototype>(sequences arg)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define haplotypes (let ((arg (simulate p markers 100))) (sequences arg)))</example>
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
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define intervals (let ((arg (simulate p markers 100))) (intervals arg)))</example>
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
(define trees (let ((arg (simulate p markers 100))) (local-trees arg)))</example>
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
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate p markers 100))
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
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate p markers 100))
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
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate p markers 100))
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

    scm_c_define_gsubr("c-simulate", 6, 0, 0, 
		       (scm_unused_struct*(*)())simulate);
    scm_c_eval_string("(read-set! keywords 'prefix)"
		      "(use-modules (ice-9 optargs))"
		      "(define (simulate p ms n . args)"
		      "  (let-keywords args #f ((coalescence-callback '())"
		      "                         (recombination-callback '())"
		      "                         (geneconversion-callback '()))"
		      "		(c-simulate p ms n"
		      "                     coalescence-callback"
		      "                     recombination-callback"
		      "                     geneconversion-callback)))");

    scm_c_define_gsubr("save-sequences", 2, 0, 0, 
		       (scm_unused_struct*(*)())save_sequences);
    scm_c_define_gsubr("sequences", 1, 0, 0, 
		       (scm_unused_struct*(*)())sequences);

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

