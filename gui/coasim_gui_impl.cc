#include "coasim_gui_impl.hh"
#include "add_marker_impl.hh"
#include "run_simulation_impl.hh"


#include <qlineedit.h>



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

// start simulation
void CoasimGuiImpl::simulate()
{
  int    no_leaves = _ln->text().toInt();
  double recomb_rate = _rr->text().toDouble();
  double geneconv_rate = _gr->text().toDouble();
  double geneconv_length = _gl->text().toDouble();
  double growth = _g->text().toDouble();
  double mrate = _mr->text().toDouble();

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
