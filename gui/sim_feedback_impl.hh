#ifndef SIM_FEEDBACK_IMPL_HH_INCLUDED
#define SIM_FEEDBACK_IMPL_HH_INCLUDED

#ifndef SIMFEEDBACKFORM_H_INCLUDED
# include "simfeedbackform.h"
# define SIMFEEDBACKFORM_H_INCLUDED
#endif
#ifndef MONITOR_HH_INCLUDED
# include <coasim/monitor.hh>
#endif

class CoasimGuiImpl;

class SimFeedbackImpl : public SimFeedbackForm, public SimulationMonitor
{
  Q_OBJECT

  // ref back to main window -- needed to access marker table and
  // provide feedback back to main window.
  CoasimGuiImpl &_main_window;

  // we need to remember this between start_arg_building and
  // builder_update/terminate.
  unsigned int _no_leaves;

  bool _abort;         // set if aborting, abort as soon as we can
  void handle_abort(); // close dialog and report abort back to simulator

public:
  SimFeedbackImpl(CoasimGuiImpl &main_window,
		  const char* name = 0, WFlags fl = 0);
  ~SimFeedbackImpl();

  void reset();

  // slots
  virtual void abort_simulation();

  // Monitor callbacks...
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
