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
#ifndef CORE__MONITOR_HH_INCLUDED
# include <Core/monitor.hh>
#endif
#ifndef CORE__NODE_HH_INCLUDED
# include <Core/node.hh>
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
scm_t_bits guile::interval_tag;

namespace {
    struct ARGData {
	const core::ARG *arg;
	const core::Configuration *conf;
	ARGData(core::ARG *arg, core::Configuration *conf)
	    : arg(arg), conf(conf)
	{};
	~ARGData()
	{
	    delete arg;
	    delete conf;
	}
    };

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
}

static size_t
free_arg(SCM s_arg_data)
{
    ARGData *arg_data = (ARGData*) SCM_SMOB_DATA(s_arg_data);
    arg_data->~ARGData();
    scm_must_free(arg_data);
    return sizeof(ARGData);
}


static SCM
mark_interval (SCM s_interval)
{
    IntervalData *interval = (IntervalData*) SCM_SMOB_DATA(s_interval);
    scm_gc_mark(interval->arg);
    return SCM_BOOL_F;
}

static size_t
free_interval(SCM s_interval)
{
    IntervalData *interval = (IntervalData*) SCM_SMOB_DATA(s_interval);
    interval->~IntervalData();
    scm_must_free(interval);
    return sizeof(IntervalData);
}


namespace {
    class CLISimMonitor : public core::SimulationMonitor {
	void start_arg_building(unsigned int no_leaves);
	void builder_update(unsigned int no_nodes, unsigned int no_top_nodes,
			    unsigned long int no_iterations, double cur_time,
			    unsigned int no_coal_events,
			    unsigned int no_gene_conv_events,
			    unsigned int no_recomb_events);
	void builder_termination(unsigned int no_nodes, unsigned int no_top_nodes,
				 unsigned long int no_iterations, double cur_time,
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


/* --<GUILE COMMENT>---------------------------------------------

<method name="simulate">
  <brief>Simulate an ARG and corresponding sequences.</brief>
  <prototype>(simulate arg-parameters marker-list no-leaves)</prototype>
  <example>(define p (arg-parameters rho Q G beta))
(define markers (make-random-snp-markers 10 0.1 0.9))
(define arg (simulate p markers 100))</example>
  <description>
    <p>
      Simulate an ARG and corresponding sequences, based ARG parameters, 
      a list of markers, and the number of markers to simulate.
    </p>
  </description>
</method>

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
simulate(SCM arg_parameters_smob, SCM s_markers, SCM s_no_leaves)
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


    try {
	CLISimMonitor mon;
	core::Configuration *conf 
	    = new core::Configuration(no_leaves,
				      markers.begin(), markers.end(),
				      p->rho, p->Q, p->G, p->growth);
	core::ARG *arg = core::Simulator::simulate(*conf,
						   options::verbose ? &mon:0);

	void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
	ARGData *arg_data = new(mem)ARGData(arg,conf);
    
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

-----</GUILE COMMENT>-------------------------------------------- */
static SCM
wrap_interval(SCM arg, const core::RetiredInterval *rinterval)
{
    void *mem = scm_must_malloc(sizeof(ARGData), "simulate");
    IntervalData *interval = new(mem)IntervalData(arg,rinterval);
    SCM_RETURN_NEWSMOB(guile::interval_tag, interval);
}
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
static SCM
interval_start(SCM interval_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
	       interval_smob, SCM_ARG1, "interval-start");
    IntervalData *interval_data = (IntervalData*) SCM_SMOB_DATA(interval_smob);
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
static SCM
interval_end(SCM interval_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
	       interval_smob, SCM_ARG1, "interval-end");
    IntervalData *interval_data = (IntervalData*) SCM_SMOB_DATA(interval_smob);
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
static SCM
total_branch_length(SCM interval_smob)
{
    SCM_ASSERT(SCM_SMOB_PREDICATE(guile::interval_tag, interval_smob),
	       interval_smob, SCM_ARG1, "interval-end");
    IntervalData *interval_data = (IntervalData*) SCM_SMOB_DATA(interval_smob);
    return scm_make_real(interval_data->rinterval->surface());
}


void
guile::install_simulate()
{
    guile::arg_tag = scm_make_smob_type("arg", sizeof(ARGData));
    scm_set_smob_free(guile::arg_tag, free_arg);

    // FIXME: add printing of intervals
    guile::interval_tag = scm_make_smob_type("interval", sizeof(IntervalData));
    scm_set_smob_mark(guile::interval_tag, mark_interval);
    scm_set_smob_free(guile::interval_tag, free_interval);

    scm_c_define_gsubr("simulate", 3, 0, 0, 
		       (scm_unused_struct*(*)())simulate);
    scm_c_define_gsubr("save-sequences", 2, 0, 0, 
		       (scm_unused_struct*(*)())save_sequences);
    scm_c_define_gsubr("sequences", 1, 0, 0, 
		       (scm_unused_struct*(*)())sequences);

    scm_c_define_gsubr("intervals", 1, 0, 0, 
		       (scm_unused_struct*(*)())intervals);
    scm_c_define_gsubr("interval-start", 1, 0, 0, 
		       (scm_unused_struct*(*)())interval_start);
    scm_c_define_gsubr("interval-end", 1, 0, 0, 
		       (scm_unused_struct*(*)())interval_end);
    scm_c_define_gsubr("total-branch-length", 1, 0, 0, 
		       (scm_unused_struct*(*)())total_branch_length);
}


