#include "add_marker_impl.hh"

#include <qtable.h>
#include <qlineedit.h>

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

  _marker_table->setText(0,0,trait_pos->text());
  _marker_table->setText(0,1,"trait");
  _marker_table->setText(0,2,trait_low_freq->text());
  _marker_table->setText(0,3,trait_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);
}

void AddMarkerImpl::add_snp()
{
  _marker_table->insertRows(0);

  _marker_table->setText(0,0,snp_pos->text());
  _marker_table->setText(0,1,"snp");
  _marker_table->setText(0,2,snp_low_freq->text());
  _marker_table->setText(0,3,snp_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);
}

void AddMarkerImpl::add_ms()
{
  _marker_table->insertRows(0);

  _marker_table->setText(0,0,ms_pos->text());
  _marker_table->setText(0,1,"ms");
  _marker_table->setText(0,2,"");
  _marker_table->setText(0,3,"");
  _marker_table->setText(0,4,ms_size->text());

  _marker_table->sortColumn(0,true,true);
}
