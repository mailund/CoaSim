
#include "coasim_gui_impl.hh"

#ifndef ADD_MARKER_IMPL_HH_INCLUDED
# include "add_marker_impl.hh"
#endif
#ifndef SHOW_SIM_RESULTS_IMPL_HH_INCLUDED
# include "show_sim_results_impl.hh"
#endif

#ifndef CONFIGURATION_HH_INCLUDED
# include <configuration.hh>
#endif
#ifndef NODE_HH_INCLUDED
# include <node.hh>
#endif
#ifndef SIMULATOR_HH_INCLUDED
# include <simulator.hh>
#endif
#ifndef ALL_MARKERS_HH_INCLUDED
# include <all_markers.hh>
#endif

#ifndef QTABLE_H_INCLUDED
# include <qtable.h>
# define QTABLE_H_INCLUDED
#endif
#ifndef QSPINBOX_H_INCLUDED
# include <qspinbox.h>
# define QSPINBOX_H_INCLUDED
#endif
#ifndef QMESSAGEBOX_H_INCLUDED
# include <qmessagebox.h>
# define QMESSAGEBOX_H_INCLUDED
#endif
#ifndef QAPPLICATION_H_INCLUDED
# include <qapplication.h>
# define QAPPLICATION_H_INCLUDED
#endif

#ifndef BAPS_FLOAT_SPIN_BOX_HH_INCLUDED
# include "baps_float_spin_box.hh"
#endif

#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif
#ifndef VECTOR_INCLUDED
# include <vector>
# define VECTOR_INCLUDED
#endif
#ifndef CMATH_INCLUDED
# include <cmath>
# define CMATH_INCLUDED
#endif

#ifndef SYS_TIME_H_INCLUDED
# include <sys/time.h>
# define SYS_TIME_H_INCLUDED
#endif

// global var defined in coasim.cc -- needed to process events while
// simulating
extern QApplication *coasim_main_app;



/* 
 *  Constructs a CoasimGuiImpl which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 */
CoasimGuiImpl::CoasimGuiImpl( QWidget* parent,  const char* name, WFlags fl )
  : CoasimGuiForm( parent, name, fl ),
    i_monitor(*this)
{
}

/*  
 *  Destroys the object and frees any allocated resources
 */
CoasimGuiImpl::~CoasimGuiImpl()
{
    // no need to delete child widgets, Qt does it all for us
}

// add a marker to the marker list
void CoasimGuiImpl::add_marker()
{
  static AddMarkerImpl * ami = 0;
  if (!ami) ami = new AddMarkerImpl(i_marker_table, this);
  ami->show();
}


// delete selected markers
void CoasimGuiImpl::delete_marker()
{
  std::vector<int> to_delete;
  int n = i_marker_table->numSelections();
  while (n-- > 0)
    {
      QTableSelection s = i_marker_table->selection(n);
      for (int row = s.bottomRow(); row >= s.topRow(); --row)
	to_delete.push_back(row);
    }

  // deleting a row renumbers the lower rows, so we have to delete
  // them from the highest and down.
  std::sort(to_delete.begin(), to_delete.end());
  std::vector<int>::reverse_iterator i;
  for (i = to_delete.rbegin(); i != to_delete.rend(); ++i)
    i_marker_table->removeRow(*i);
}


// start simulation
void CoasimGuiImpl::simulate()
{
  // configure simulation
  int    no_leaves = i_ln->text().toInt();
  double recomb_rate =   i_recomb_rate->text().toDouble();
  double geneconv_rate = i_gr->text().toDouble();
  double geneconv_length = i_gl->text().toDouble();
  double growth = i_growth->text().toDouble();

  std::vector<double> positions(i_marker_table->numRows());
  std::vector<Marker*> markers(i_marker_table->numRows());
 
  for (int i = 0; i < i_marker_table->numRows(); ++i)
    positions[i] = i_marker_table->text(i,0).toDouble();

  Configuration conf(no_leaves,
		     positions.begin(), positions.end(),
		     recomb_rate,
		     geneconv_rate, geneconv_length,
		     growth,
		     &i_monitor);

  for (int i = 0; i < i_marker_table->numRows(); ++i)
    {
      if (i_marker_table->text(i,1) == "trait")
	{
	  markers[i] = new TraitMarker(i_marker_table->text(i,2).toDouble(),
				       i_marker_table->text(i,3).toDouble());
	  conf.set_marker(i,markers[i],true);
	}
      else if (i_marker_table->text(i,1) == "snp")
	{
	  markers[i] = new SNPMarker(i_marker_table->text(i,2).toDouble(),
				     i_marker_table->text(i,3).toDouble());
	  conf.set_marker(i,markers[i]);
	}
      else
	{
	  int alpha_size = i_marker_table->text(i,4).toInt();
	  double mrate = i_marker_table->text(i,5).toDouble();
	  MicroSatelliteMarker *m = new MicroSatelliteMarker(mrate);
	  for (int j = 0; j < alpha_size; ++j) m->add_value(j);
	  markers[i] = m;
	  conf.set_marker(i,markers[i]);
	}
    }

  try {

    i_monitor.show(); i_monitor.reset();
    while (coasim_main_app->hasPendingEvents())
      coasim_main_app->processEvents();


    struct timeval tv; struct timezone tz;
    gettimeofday(&tv,&tz);
    std::srand(tv.tv_usec);
    ARG *arg = Simulator::simulate(conf);

    if (!arg)
      {
	// simulation aborted
      }
    else
      {
	ShowSimResultsImpl show_results_dialog(conf,*arg,this);
	show_results_dialog.exec();
      }

    delete arg;

  } catch (std::exception &ex) {
    QMessageBox::critical(this, "Unexpected Error",
			  QString("Unexpected exception \"")
			  .append(ex.what())
			  .append("\" raised while simulating!"));
  }

  for (int i = 0; i < i_marker_table->numRows(); ++i)
    delete markers[i];
}
