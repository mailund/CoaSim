
#include "run_simulation_impl.hh"

#include <coasim/configuration.hh>
#include <coasim/all_markers.hh>
#include <coasim/node.hh>
#include <coasim/simulator.hh>

#include <qtable.h>
#include <qcheckbox.h>
#include <qlineedit.h>
#include <qfiledialog.h>
#include <qmessagebox.h>

#include <fstream>
#include <vector>

RunSimulationImpl::RunSimulationImpl(QTable *marker_table,
				     int no_leaves,
				     double recomb_rate,
				     double geneconv_rate, 
				     double geneconv_length,
				     double growth,
				     double mrate,
				     QWidget* parent,  const char* name, WFlags fl )
  : RunSimulationForm( parent, name, fl ),
    _marker_table(marker_table),
    _no_leaves(no_leaves),
    _recomb_rate(recomb_rate),
    _geneconv_rate(geneconv_rate),
    _geneconv_length(geneconv_length),
    _growth(growth),
    _mrate(mrate)
{
}

RunSimulationImpl::~RunSimulationImpl()
{
  // no need to delete child widgets, Qt does it all for us
}

void RunSimulationImpl::set_out_file()
{
  QString fname = QFileDialog::getSaveFileName();
  if (fname == "") return; // cancel
  xml_file_name->setText(fname);
}


void RunSimulationImpl::run_simulation()
{
  QString fname = xml_file_name->text();

 retry:
  std::ofstream xml_file(fname);
  if (!xml_file)
    {
      if (QMessageBox::warning(this, "Error opening file",
			       QString("Could not open file ")
			       .append(fname),
			       "Retry", "Quit",
			       0, 0, 1) == 0)
	{
	  fname = QFileDialog::getSaveFileName();
	  if (fname == "") return; // cancel    
	  goto retry;
	}
      return;
    }

  std::vector<double> positions(_marker_table->numRows());
  std::vector<Marker*> markers(_marker_table->numRows());

  bool leaves_only = mode->isOn();

  try {
  
    for (int i = 0; i < _marker_table->numRows(); ++i)
      positions[i] = _marker_table->text(i,0).toDouble();

    Configuration conf(_no_leaves,
		       positions.begin(), positions.end(),
		       _recomb_rate,
		       _geneconv_rate, _geneconv_length,
		       _growth,
		       _mrate,
		       !leaves_only);

    for (int i = 0; i < _marker_table->numRows(); ++i)
      {
	if (_marker_table->text(i,1) == "trait")
	  markers[i] = new TraitMarker(_marker_table->text(i,2).toDouble(),
				       _marker_table->text(i,3).toDouble());
	else if (_marker_table->text(i,1) == "snp")
	  markers[i] = new SNPMarker(_marker_table->text(i,2).toDouble(),
				     _marker_table->text(i,3).toDouble());
	else
	  {
	    MicroSatelliteMarker *m = new MicroSatelliteMarker(_mrate);
	    int alpha_size = _marker_table->text(i,4).toInt();
	    for (int j = 0; j < alpha_size; ++j) m->add_value(j);
	    markers[i] = m;
	  }
      
	conf.set_marker(i,markers[i]);
      }

    ARG *arg = Simulator::simulate(conf);
    xml_file << *arg << std::endl;


  } catch (std::exception &ex) {
    QMessageBox::critical(this, "Unexpected Error",
			  QString("Unexpected exception \"")
			  .append(ex.what())
			  .append("\" raised while simulating!"));
  }

  for (int i = 0; i < _marker_table->numRows(); ++i)
    delete markers[i];

  close();			// close run sim. dialog
}
