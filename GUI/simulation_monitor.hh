#ifndef SIMULATION_MONITOR_HH_INCLUDED
#define SIMULATION_MONITOR_HH_INCLUDED

#include <Core/monitor.hh>
#include <qobject.h>

class SimulationDialog;
namespace core {
    class Configuration;
}

class Monitor : public core::SimulationMonitor, 
		public QObject /* to get tr() */ {
    bool i_aborting;

    int i_no_leaves;

    SimulationDialog *i_feedback;
    core::Configuration *i_conf;

    void process_pending_events();
    void handle_abort();


public:
    Monitor(SimulationDialog *feedback_dialog, core::Configuration *conf) 
	: i_aborting(false), i_no_leaves(0), 
	  i_feedback(feedback_dialog), i_conf(conf)
    {}
    virtual ~Monitor();

    void run();

    void abort() { i_aborting = true; }
    bool aborting() const { return i_aborting; }

    virtual void start_arg_building(unsigned int no_leaves);
    virtual void builder_update(unsigned int no_nodes,
				unsigned int no_top_nodes,
				unsigned long int no_iterations,
				double cur_time,
				unsigned int no_coal_events,
				unsigned int no_gene_conv_events,
				unsigned int no_recomb_events);

    virtual void builder_termination(unsigned int no_nodes,
				     unsigned int no_top_nodes,
				     unsigned long int no_iterations,
				     double cur_time,
				     unsigned int no_coal_events,
				     unsigned int no_gene_conv_events,
				     unsigned int no_recomb_events);
  
    virtual void start_mutating();
    virtual void mutator_update(unsigned int marker_no);
    virtual void retry_mutation();
    virtual void retry_arg_building();

    virtual void simulation_terminated();
};




#endif
