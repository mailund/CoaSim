#include "coasim_gui_impl.hh"
#include "add_marker_impl.hh"
#include "run_simulation_impl.hh"

#include <coasim/configuration.hh>
#include <coasim/all_markers.hh>
#include <coasim/node.hh>
#include <coasim/simulator.hh>

#include <qspinbox.h>
#include <qtable.h>
#include <qmessagebox.h>

#include "baps_float_spin_box.hh"

#include <vector>
#include <fstream>



/* 
 *  Constructs a CoasimGuiImpl which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 */
CoasimGuiImpl::CoasimGuiImpl( QWidget* parent,  const char* name, WFlags fl )
  : CoasimGuiForm( parent, name, fl ),
    _monitor(this)
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
  if (!ami) ami = new AddMarkerImpl(_marker_table, this);
  ami->show();
}


// delete selected markers
void CoasimGuiImpl::delete_marker()
{
  std::vector<int> to_delete;
  int n = _marker_table->numSelections();
  while (n-- > 0)
    {
      QTableSelection s = _marker_table->selection(n);
      for (int row = s.bottomRow(); row >= s.topRow(); --row)
	to_delete.push_back(row);
    }

  // deleting a row renumbers the lower rows, so we have to delete
  // them from the highest and down.
  std::sort(to_delete.begin(), to_delete.end());
  std::vector<int>::reverse_iterator i;
  for (i = to_delete.rbegin(); i != to_delete.rend(); ++i)
    _marker_table->removeRow(*i);
}


// start simulation
void CoasimGuiImpl::simulate()
{

  QString outfile;
  bool    leaves_only;

  RunSimulationImpl run_dialog(outfile,leaves_only,this);
  run_dialog.exec();

  if (outfile == "") /* abort */ return;


  // configure simulation
  int    no_leaves = _ln->text().toInt();
  double recomb_coef = _rr_coef->text().toDouble();
  double recomb_exp  = _rr_exp->text().toDouble();
  double recomb_rate = recomb_coef * ::pow(10,recomb_exp);
  double geneconv_rate = _gr->text().toDouble();
  double geneconv_length = _gl->text().toDouble();
  double growth = _g->text().toDouble();
  double mrate_coef = _mr_coef->text().toDouble();
  double mrate_exp  = _mr_exp->text().toDouble();
  double mrate =  mrate_coef * ::pow(10,mrate_exp);

  std::vector<double> positions(_marker_table->numRows());
  std::vector<Marker*> markers(_marker_table->numRows());

 
  for (int i = 0; i < _marker_table->numRows(); ++i)
    positions[i] = _marker_table->text(i,0).toDouble();

  Configuration conf(no_leaves,
		     positions.begin(), positions.end(),
		     recomb_rate,
		     geneconv_rate, geneconv_length,
		     growth,
		     mrate,
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
	  MicroSatelliteMarker *m = new MicroSatelliteMarker(mrate);
	  int alpha_size = _marker_table->text(i,4).toInt();
	  for (int j = 0; j < alpha_size; ++j) m->add_value(j);
	  markers[i] = m;
	}
      
      conf.set_marker(i,markers[i]);
    }

  try {

    _monitor.show(); // FIXME reset mon

    std::ofstream xml_file(outfile);
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
}
