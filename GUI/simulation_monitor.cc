
#include "simulation_monitor.hh"
#include "simulationdialog.h" // a bit ugly here...cyclic dependencies...

#include "simresultsdialog.h"

#include <Core/configuration.hh>
#include <Core/simulator.hh>
#include <Core/node.hh>

#include <qtextedit.h>
#include <qlineedit.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qprogressbar.h>
#include <qtable.h>

void Monitor::handle_abort()
{
    if (aborting()) throw core::SimulationMonitor::AbortSimulation();

}

void Monitor::process_pending_events()
{
    extern QApplication *coasim_main_app;

    while (coasim_main_app->hasPendingEvents())
	coasim_main_app->processEvents();

    handle_abort();
}

Monitor::~Monitor()
{
    delete i_conf;
}

void Monitor::run()
{
    // FIXME: run this in separate thread!
    core::ARG *arg = core::Simulator::simulate(*i_conf, this);
    if (!arg) /* aborted */ i_feedback->close();

    SimResultsDialog res(i_feedback);

    res.i_arg = arg;
    unsigned int no_markers    = i_conf->no_markers();
    unsigned int no_haplotypes = i_conf->no_leaves();
    res.haplotypeTable->setNumCols(no_markers);
    res.haplotypeTable->setNumRows(no_haplotypes);

    for (unsigned int h = 0; h < no_haplotypes; ++h)
	for (unsigned int m = 0; m < no_markers; ++m)
	    res.haplotypeTable->setText(h,m, 
					QString("%1")
					.arg(arg->leaves()[h]->state(m)));


    // FIXME: fill out table...
    res.exec();
    delete arg;
}

void Monitor::start_arg_building(unsigned int no_leaves)
{
    i_no_leaves = no_leaves;
    i_feedback->noLeafEvents->setText(QString("%1").arg(no_leaves));
    i_feedback->simLog->append("Building ARG\n");

    process_pending_events();
}

void Monitor::builder_update(unsigned int no_nodes,
			     unsigned int no_top_nodes,
			     unsigned long int /*no_iterations*/,
			     double cur_time,
			     unsigned int no_coal_events,
			     unsigned int no_gene_conv_events,
			     unsigned int no_recomb_events)
{
    i_feedback->totalNodes->setText(QString("%1").arg(no_nodes));
    i_feedback->topNodes->setText(QString("%1").arg(no_top_nodes));
    i_feedback->currentTime->setText(QString("%1").arg(cur_time));

    i_feedback->noCoaEvents->setText(QString("%1").arg(no_coal_events));
    i_feedback->noRecombEvents->setText(QString("%1").arg(no_recomb_events));
    i_feedback->noGeneConvEvents->setText(QString("%1").arg(no_gene_conv_events));
    
    unsigned int max_events = std::max(std::max(static_cast<unsigned int>(i_no_leaves),
						no_coal_events),
				       std::max(no_gene_conv_events,
						no_recomb_events));
    
    i_feedback->leafNodesProgress->setProgress(i_no_leaves,max_events);
    i_feedback->coaEventsProgress->setProgress(no_coal_events,max_events);
    i_feedback->recombEventsProgress->setProgress(no_recomb_events,max_events);
    i_feedback->geneConvEventsProgress->setProgress(no_gene_conv_events,max_events);

    process_pending_events();
}

void Monitor::builder_termination(unsigned int no_nodes,
				  unsigned int no_top_nodes,
				  unsigned long int /*no_iterations*/,
				  double cur_time,
				  unsigned int no_coal_events,
				  unsigned int no_gene_conv_events,
				  unsigned int no_recomb_events)
{
    i_feedback->totalNodes->setText(QString("%1").arg(no_nodes));
    i_feedback->topNodes->setText(QString("%1").arg(no_top_nodes));
    i_feedback->currentTime->setText(QString("%1").arg(cur_time));

    i_feedback->noCoaEvents->setText(QString("%1").arg(no_coal_events));
    i_feedback->noRecombEvents->setText(QString("%1").arg(no_recomb_events));
    i_feedback->noGeneConvEvents->setText(QString("%1").arg(no_gene_conv_events));
    
    unsigned int max_events = std::max(std::max(static_cast<unsigned int>(i_no_leaves),
						no_coal_events),
				       std::max(no_gene_conv_events,
						no_recomb_events));
    
    i_feedback->leafNodesProgress->setProgress(i_no_leaves,max_events);
    i_feedback->coaEventsProgress->setProgress(no_coal_events,max_events);
    i_feedback->recombEventsProgress->setProgress(no_recomb_events,max_events);
    i_feedback->geneConvEventsProgress->setProgress(no_gene_conv_events,max_events);

    process_pending_events();
}
  
void Monitor::start_mutating()
{
    i_feedback->simLog->append("Mutating ARG\n");
    process_pending_events();
}

void Monitor::mutator_update(unsigned int marker_no)
{
    i_feedback->markerNo->setText(QString("%1").arg(marker_no));
    i_feedback->markerType->setText(i_conf->marker(marker_no).type());
    i_feedback->markerType->setText(QString("%1").arg(i_conf->marker(marker_no).position()));

    process_pending_events();
}

void Monitor::retry_mutation()
{
    i_feedback->simLog->setColor("gray");
    i_feedback->simLog->setItalic(true);
    i_feedback->simLog->append("Mutation outside frequency bounds, retrying\n");
    i_feedback->simLog->setItalic(false);
    i_feedback->simLog->setColor("black");

    process_pending_events();
}

void Monitor::retry_arg_building()
{
    i_feedback->simLog->setColor("red");
    i_feedback->simLog->setItalic(true);
    i_feedback->simLog->append("Trait mutation outside bounds, rebilding arg...\n");
    i_feedback->simLog->setItalic(false);
    i_feedback->simLog->setColor("black");

    process_pending_events();
}

void Monitor::simulation_terminated()
{
    i_feedback->simLog->setBold(true);
    i_feedback->simLog->append("Simulation completed.\n");
    i_feedback->simLog->setBold(false);

    // no need to show last marker...
    i_feedback->markerNo->setText("");
    i_feedback->markerType->setText("");
    i_feedback->markerPosition->setText("");

    i_feedback->abortButton->setDisabled(true);
    i_feedback->closeButton->setDisabled(false);

    process_pending_events();
}
