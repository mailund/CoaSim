/****************************************************************************
 ** ui.h extension file, included from the uic-generated form implementation.
 **
 ** If you want to add, delete, or rename functions or slots, use
 ** Qt Designer to update this file, preserving your code.
 **
 ** You should not define a constructor or destructor in this file.
 ** Instead, write your code in functions called init() and destroy().
 ** These will automatically be called by the form's constructor and
 ** destructor.
 *****************************************************************************/

#include <Core/monitor.hh>
#include <Core/all_markers.hh>
#include <Core/configuration.hh>

#include "maindialog.h"
#include "simulation_monitor.hh"

#include <baps_float_spin_box.hh>
#include <qspinbox.h>
#include <qtable.h>

#include <iostream>

namespace {
    struct marker_less :
	std::binary_function<core::Marker*,core::Marker*,bool> {
	bool operator() (const core::Marker* m1, const core::Marker *m2) const
	{
	    return m1->position() < m2->position();
	}
    };
}

void SimulationDialog::init()
{
    aborting = false;

    MainDialog *main = dynamic_cast<MainDialog*>(parentWidget());

    int no_leaves = main->noChromosomes->text().toInt();
    double rho    = main->rho->text().toDouble();
    double Q      = 0; 		// FIXME
    double G      = 0; 		// FIXME
    double growth = 0; 		// FIXME

    std::vector<core::Marker*> markers;

    for (int i = 0; i < main->traitMarkerTable->numRows(); ++i)
	{
	    double pos  = main->traitMarkerTable->text(i,0).toDouble();
	    double low  = main->traitMarkerTable->text(i,1).toDouble();
	    double high = main->traitMarkerTable->text(i,2).toDouble();
	    markers.push_back(new core::TraitMarker(pos, low, high));
	}
    for (int i = 0; i < main->SNPMarkerTable->numRows(); ++i)
	{
	    double pos  = main->SNPMarkerTable->text(i,0).toDouble();
	    double low  = main->SNPMarkerTable->text(i,1).toDouble();
	    double high = main->SNPMarkerTable->text(i,2).toDouble();
	    markers.push_back(new core::SNPMarker(pos, low, high));
	}
    for (int i = 0; i < main->MSMarkerTable->numRows(); ++i)
	{
	    double pos     = main->MSMarkerTable->text(i,0).toDouble();
	    int alpha_size = main->MSMarkerTable->text(i,1).toInt();
	    double mu      = main->MSMarkerTable->text(i,2).toDouble();

	    core::MicroSatelliteMarker *m = 
		new core::MicroSatelliteMarker(pos, mu);
	    for (int j = 0; j < alpha_size; ++j) m->add_value(j);

	    markers.push_back(m);
	}

    std::sort(markers.begin(), markers.end(), marker_less());

    simMonitor = new Monitor(this,
			     new core::Configuration(no_leaves,
						     markers.begin(), 
						     markers.end(),
						     rho, Q, G, growth));

    std::vector<core::Marker*>::iterator m_itr;
    for (m_itr = markers.begin(); m_itr != markers.end(); ++m_itr)
	delete *m_itr;

    show();
    simMonitor->run();
}

void SimulationDialog::destroy()
{
    delete simMonitor;
    simMonitor = 0;
}


void SimulationDialog::abortSimulation()
{
    // simply set flag to inform simulator of aborting
    assert(simMonitor);
    simMonitor->abort();
}


