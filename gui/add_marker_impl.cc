#include "add_marker_impl.hh"

#include <qtable.h>
#include <qlineedit.h>
#include <qspinbox.h>
#include <qmessagebox.h>
#include <qslider.h>

#include "baps_float_spin_box.hh"


// FIXME: it is hardwired througout this module, that the positions
// are 1/1000 of the integer positions we operate on.  Working on the
// floats directly might cause rounding errors when converting to and
// from inters, doubles, and strings, which is why we only operate on
// the integers.



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


bool AddMarkerImpl::check_position(int pos) const
{
  // FIXME: if this turns out to be too slow, use binary search -- we
  // know the positions are sorted.
  int no_rows = _marker_table->numRows();
  for (int i = 0; i < no_rows; ++i)
    {
      // text field == "0.xxx" where xxx is the int we want
      int row_pos = _marker_table->text(i,0).mid(2).toInt();
      if (row_pos == pos)
	{
	  // the pos is not allowed; it is already taken
	  QMessageBox::warning(0, "Position Error",
			       "The chosen position is already occupied!",
			       "OK");
	  return false;
	}
      if (row_pos > pos) break; // no problem with later positions
    }
  return true;
}



void AddMarkerImpl::add_trait()
{
  int pos = _trait_pos->value();
  if (!check_position(pos)) return;

  _marker_table->insertRows(0);

  _marker_table->setText(0,0,QString("").sprintf("0.%03d",pos));
  _marker_table->setText(0,1,"trait");
  _marker_table->setText(0,2,_trait_low_freq->text());
  _marker_table->setText(0,3,_trait_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);

  _trait_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::add_snp()
{
  int pos = _snp_pos->value();
  if (!check_position(pos)) return;

  _marker_table->insertRows(0);

  _marker_table->setText(0,0,QString("").sprintf("0.%03d",pos));
  _marker_table->setText(0,1,"snp");
  _marker_table->setText(0,2,_snp_low_freq->text());
  _marker_table->setText(0,3,_snp_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);

  _snp_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::add_ms()
{
  int pos = _ms_pos->value();
  if (!check_position(pos)) return;

  _marker_table->insertRows(0);

  _marker_table->setText(0,0,QString("").sprintf("0.%03d",pos));
  _marker_table->setText(0,1,"ms");
  _marker_table->setText(0,2,"");
  _marker_table->setText(0,3,"");
  _marker_table->setText(0,4,_ms_size->text());

  _marker_table->sortColumn(0,true,true);

  _ms_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::next_pos_changed(int pos)
{
  _next_pos->setText(QString("").sprintf("0.%03d",pos));
  _trait_pos->setValue(pos);
  _snp_pos  ->setValue(pos);
  _ms_pos   ->setValue(pos);
}


int AddMarkerImpl::update_next_position(int pos)
{
  int cur_next_pos = _next_pos_slider->value();
  if (pos != cur_next_pos) return cur_next_pos;

  int step = _next_pos_step->value();
  int next_pos = cur_next_pos + step;

  // move position -- this automatically updates all that depends on it.
  _next_pos_slider->setValue(next_pos);

  return next_pos;
}
