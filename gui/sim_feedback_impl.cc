
#include "sim_feedback_impl.hh"

#ifndef COASIM_GUI_IMPL_HH_INCLUDED
# include "coasim_gui_impl.hh"
#endif

#ifndef QPUSHBUTTON_H_INCLUDED
# include <qpushbutton.h>
# define QPUSHBUTTON_H_INCLUDED
#endif
#ifndef QTEXTEDIT_H_INCLUDED
# include <qtextedit.h>
# define QTEXTEDIT_H_INCLUDED
#endif
#ifndef QLINEEDIT_H_INCLUDED
# include <qlineedit.h>
# define QLINEEDIT_H_INCLUDED
#endif
#ifndef QPROGRESSBAR_H_INCLUDED
# include <qprogressbar.h>
# define QPROGRESSBAR_H_INCLUDED
#endif
#ifndef QTABLE_H_INCLUDED
# include <qtable.h>
# define QTABLE_H_INCLUDED
#endif

#ifndef QAPPLICATION_H_INCLUDED
# include <qapplication.h>
# define QAPPLICATION_H_INCLUDED
#endif


// global var defined in coasim.cc -- needed to process events while
// simulating
extern QApplication *coasim_main_app;


SimFeedbackImpl::SimFeedbackImpl(CoasimGuiImpl &main_window,
				 const char* name, 
				 WFlags fl)
  : SimFeedbackForm( &main_window, name, fl ),
    i_main_window(main_window),
    i_no_leaves(0), i_abort(false)
{
}

SimFeedbackImpl::~SimFeedbackImpl()
{
  // no need to delete child widgets, Qt does it all for us
}

void SimFeedbackImpl::handle_abort()
{
  if (!i_abort) return; // not aborting
  close();
  throw SimulationMonitor::AbortSimulation();
}

void SimFeedbackImpl::abort_simulation()
{
  // simply flag so we know how to abort next time we can inform the
  // simulator.
  i_abort = true;
}


void SimFeedbackImpl::reset()
{
  i_abort = false;
  i_abort_button->setDisabled(false);
  i_close_button->setDisabled(true);

  i_total_nodes->setText("0");
  i_top_nodes->setText("0");
  i_time->setText("0.0");

  i_no_leaf_events->setText("");
  i_coal_events->setText("");
  i_recomb_events->setText("");
  i_gene_conv_events->setText("");

  i_leaf_nodes_progress->setProgress(0,1);
  i_coal_events_progress->setProgress(0,1);
  i_recomb_events_progress->setProgress(0,1);
  i_gene_conv_events_progress->setProgress(0,1);

  i_marker_no->setText("");
  i_marker_type->setText("");
  i_marker_pos->setText("");

  i_sim_log->setText("");

}


// Monitor callbacks...
void SimFeedbackImpl::start_arg_building(unsigned int no_leaves)
{
  i_no_leaves = no_leaves;
  i_no_leaf_events->setText(QString("%1").arg(no_leaves));
  
  i_sim_log->append("Building ARG\n");

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::builder_update(unsigned int no_nodes,
				     unsigned int no_top_nodes,
				     unsigned long int,
				     double cur_time,
				     unsigned int no_coal_events,
				     unsigned int no_gene_conv_events,
				     unsigned int no_recomb_events)
{
  i_total_nodes->setText(QString("%1").arg(no_nodes));
  i_top_nodes->setText(QString("%1").arg(no_top_nodes));
  i_time->setText(QString("%1").arg(cur_time));

  i_coal_events->setText(QString("%1").arg(no_coal_events));
  i_recomb_events->setText(QString("%1").arg(no_recomb_events));
  i_gene_conv_events->setText(QString("%1").arg(no_gene_conv_events));

  unsigned int max_events = std::max(std::max(i_no_leaves,no_coal_events),
				     std::max(no_gene_conv_events,
					      no_recomb_events));

  i_leaf_nodes_progress->setProgress(i_no_leaves,max_events);
  i_coal_events_progress->setProgress(no_coal_events,max_events);
  i_recomb_events_progress->setProgress(no_recomb_events,max_events);
  i_gene_conv_events_progress->setProgress(no_gene_conv_events,max_events);

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::builder_termination(unsigned int no_nodes,
					  unsigned int no_top_nodes,
					  unsigned long int,
					  double cur_time,
					  unsigned int no_coal_events,
					  unsigned int no_gene_conv_events,
					  unsigned int no_recomb_events)
{
  i_total_nodes->setText(QString("%1").arg(no_nodes));
  i_top_nodes->setText(QString("%1").arg(no_top_nodes));
  i_time->setText(QString("%1").arg(cur_time));

  i_coal_events->setText(QString("%1").arg(no_coal_events));
  i_recomb_events->setText(QString("%1").arg(no_recomb_events));
  i_gene_conv_events->setText(QString("%1").arg(no_gene_conv_events));

  unsigned int max_events = std::max(std::max(i_no_leaves,no_coal_events),
				     std::max(no_gene_conv_events,
					      no_recomb_events));

  i_leaf_nodes_progress->setProgress(i_no_leaves,max_events);
  i_coal_events_progress->setProgress(no_coal_events,max_events);
  i_recomb_events_progress->setProgress(no_recomb_events,max_events);
  i_gene_conv_events_progress->setProgress(no_gene_conv_events,max_events);

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::start_mutating() 
{
  i_sim_log->append("Mutating ARG\n");
  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();
}

void SimFeedbackImpl::mutator_update(unsigned int marker_no) 
{
  i_marker_no->setText(QString("%1").arg(marker_no));
  i_marker_type->setText(i_main_window.i_marker_table->text(marker_no,1));
  i_marker_pos->setText(i_main_window.i_marker_table->text(marker_no,0));

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::retry_mutation() 
{
  i_sim_log->setColor("gray");
  i_sim_log->setItalic(true);
  i_sim_log->append("Mutation outside frequency bounds, retry...\n");
  i_sim_log->setItalic(false);
  i_sim_log->setColor("black");

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::retry_arg_building() 
{
  i_sim_log->setColor("red");
  i_sim_log->setItalic(true);
  i_sim_log->append("Trait mutation outside bounds, rebilding arg...\n");
  i_sim_log->setItalic(false);
  i_sim_log->setColor("black");

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::simulation_terminated()
{
  i_sim_log->setBold(true);
  i_sim_log->append("Simulation completed.\n");
  i_sim_log->setBold(false);

  // no need to show last marker...
  i_marker_no->setText("");
  i_marker_type->setText("");
  i_marker_pos->setText("");

  i_abort_button->setDisabled(true);
  i_close_button->setDisabled(false);

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}
