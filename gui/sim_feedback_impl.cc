
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
    _main_window(main_window),
    _no_leaves(0), _abort(false)
{
}

SimFeedbackImpl::~SimFeedbackImpl()
{
  // no need to delete child widgets, Qt does it all for us
}

void SimFeedbackImpl::handle_abort()
{
  if (!_abort) return; // not aborting
  close();
  throw SimulationMonitor::AbortSimulation();
}

void SimFeedbackImpl::abort_simulation()
{
  // simply flag so we know how to abort next time we can inform the
  // simulator.
  _abort = true;
}


void SimFeedbackImpl::reset()
{
  _abort = false;
  _abort_button->setDisabled(false);
  _close_button->setDisabled(true);

  _total_nodes->setText("0");
  _top_nodes->setText("0");
  _time->setText("0.0");

  _no_leaf_events->setText("");
  _coal_events->setText("");
  _recomb_events->setText("");
  _gene_conv_events->setText("");

  _leaf_nodes_progress->setProgress(0,1);
  _coal_events_progress->setProgress(0,1);
  _recomb_events_progress->setProgress(0,1);
  _gene_conv_events_progress->setProgress(0,1);

  _marker_no->setText("");
  _marker_type->setText("");
  _marker_pos->setText("");

  _sim_log->setText("");

}


// Monitor callbacks...
void SimFeedbackImpl::start_arg_building(unsigned int no_leaves)
{
  _no_leaves = no_leaves;
  _no_leaf_events->setText(QString("%1").arg(no_leaves));
  
  _sim_log->append("Building ARG\n");

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
  _total_nodes->setText(QString("%1").arg(no_nodes));
  _top_nodes->setText(QString("%1").arg(no_top_nodes));
  _time->setText(QString("%1").arg(cur_time));

  _coal_events->setText(QString("%1").arg(no_coal_events));
  _recomb_events->setText(QString("%1").arg(no_recomb_events));
  _gene_conv_events->setText(QString("%1").arg(no_gene_conv_events));

  unsigned int max_events = std::max(std::max(_no_leaves,no_coal_events),
				     std::max(no_gene_conv_events,
					      no_recomb_events));

  _leaf_nodes_progress->setProgress(_no_leaves,max_events);
  _coal_events_progress->setProgress(no_coal_events,max_events);
  _recomb_events_progress->setProgress(no_recomb_events,max_events);
  _gene_conv_events_progress->setProgress(no_gene_conv_events,max_events);

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
  _total_nodes->setText(QString("%1").arg(no_nodes));
  _top_nodes->setText(QString("%1").arg(no_top_nodes));
  _time->setText(QString("%1").arg(cur_time));

  _coal_events->setText(QString("%1").arg(no_coal_events));
  _recomb_events->setText(QString("%1").arg(no_recomb_events));
  _gene_conv_events->setText(QString("%1").arg(no_gene_conv_events));

  unsigned int max_events = std::max(std::max(_no_leaves,no_coal_events),
				     std::max(no_gene_conv_events,
					      no_recomb_events));

  _leaf_nodes_progress->setProgress(_no_leaves,max_events);
  _coal_events_progress->setProgress(no_coal_events,max_events);
  _recomb_events_progress->setProgress(no_recomb_events,max_events);
  _gene_conv_events_progress->setProgress(no_gene_conv_events,max_events);

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::start_mutating() 
{
  _sim_log->append("Mutating ARG\n");
  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();
}

void SimFeedbackImpl::mutator_update(unsigned int marker_no) 
{
  _marker_no->setText(QString("%1").arg(marker_no));
  _marker_type->setText(_main_window._marker_table->text(marker_no,1));
  _marker_pos->setText(_main_window._marker_table->text(marker_no,0));

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::retry_mutation() 
{
  _sim_log->setColor("gray");
  _sim_log->setItalic(true);
  _sim_log->append("Trait mutation outside bounds, rebilding arg\n");
  _sim_log->setItalic(false);
  _sim_log->setColor("black");

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::retry_arg_building() 
{
  _sim_log->setColor("red");
  _sim_log->setItalic(true);
  _sim_log->append("Trait mutation outside bounds, rebilding arg\n");
  _sim_log->setItalic(false);
  _sim_log->setColor("black");

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}

void SimFeedbackImpl::simulation_terminated()
{
  _sim_log->setBold(true);
  _sim_log->append("Simulation completed.\n");
  _sim_log->setBold(false);

  // no need to show last marker...
  _marker_no->setText("");
  _marker_type->setText("");
  _marker_pos->setText("");

  _abort_button->setDisabled(true);
  _close_button->setDisabled(false);

  while (coasim_main_app->hasPendingEvents())
    coasim_main_app->processEvents();

  handle_abort(); // abort if abort flag is set
}
