#include "add_marker_impl.hh"

#include <qtable.h>
#include <qspinbox.h>
#include "baps_float_spin_box.hh"

/* 
 *  Constructs a AddMarkerImpl which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 */
AddMarkerImpl::AddMarkerImpl( QTable *marker_table,
			      QWidget* parent,  const char* name, WFlags fl )
  : AddMarkerForm( parent, name, fl ), _marker_table(marker_table)
{
}

/*  
 *  Destroys the object and frees any allocated resources
 */
AddMarkerImpl::~AddMarkerImpl()
{
    // no need to delete child widgets, Qt does it all for us
}


void AddMarkerImpl::add_trait()
{
  _marker_table->insertRows(0);

  _marker_table->setText(0,0,_trait_pos->text());
  _marker_table->setText(0,1,"trait");
  _marker_table->setText(0,2,_trait_low_freq->text());
  _marker_table->setText(0,3,_trait_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);
}

void AddMarkerImpl::add_snp()
{
  _marker_table->insertRows(0);

  _marker_table->setText(0,0,_snp_pos->text());
  _marker_table->setText(0,1,"snp");
  _marker_table->setText(0,2,_snp_low_freq->text());
  _marker_table->setText(0,3,_snp_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);
}

void AddMarkerImpl::add_ms()
{
  _marker_table->insertRows(0);

  _marker_table->setText(0,0,_ms_pos->text());
  _marker_table->setText(0,1,"ms");
  _marker_table->setText(0,2,"");
  _marker_table->setText(0,3,"");
  _marker_table->setText(0,4,_ms_size->text());

  _marker_table->sortColumn(0,true,true);
}
