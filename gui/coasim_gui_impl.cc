#include "coasim_gui_impl.hh"
#include "add_marker_impl.hh"
#include "run_simulation_impl.hh"


#include <qspinbox.h>
#include <qtable.h>
#include "baps_float_spin_box.hh"

#include <vector>



/* 
 *  Constructs a CoasimGuiImpl which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 */
CoasimGuiImpl::CoasimGuiImpl( QWidget* parent,  const char* name, WFlags fl )
    : CoasimGuiForm( parent, name, fl )
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
  if (!ami) ami = new AddMarkerImpl(markerTable, this);
  ami->show();
}


// delete selected markers
void CoasimGuiImpl::delete_marker()
{
  std::vector<int> to_delete;
  int n = markerTable->numSelections();
  while (n-- > 0)
    {
      QTableSelection s = markerTable->selection(n);
      for (int row = s.bottomRow(); row >= s.topRow(); --row)
	to_delete.push_back(row);
    }
  // deleting a row renumbers the lower rows, so we have to delete
  // them from the highest and down.
  std::sort(to_delete.begin(), to_delete.end());
  std::vector<int>::reverse_iterator i;
  for (i = to_delete.rbegin(); i != to_delete.rend(); ++i)
    markerTable->removeRow(*i);
}


// start simulation
void CoasimGuiImpl::simulate()
{
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

  RunSimulationImpl runner(markerTable,
			   no_leaves,
			   recomb_rate,
			   geneconv_rate, geneconv_length,
			   growth,
			   mrate,
			   this);

  runner.show();
  runner.exec();
}
