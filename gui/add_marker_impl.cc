#include "add_marker_impl.hh"

#ifndef QTABLE_H_INCLUDED
# include <qtable.h>
# define QTABLE_H_INCLUDED
#endif
#ifndef QLINEEDIT_H_INCLUDED
# include <qlineedit.h>
# define QLINEEDIT_H_INCLUDED
#endif
#ifndef QSPINBOX_H_INCLUDED
# include <qspinbox.h>
# define QSPINBOX_H_INCLUDED
#endif
#ifndef QMESSAGEBOX_H_INCLUDED
# include <qmessagebox.h>
# define QMESSAGEBOX_H_INCLUDED
#endif
#ifndef QSLIDER_H_INCLUDED
# include <qslider.h>
# define QSLIDER_H_INCLUDED
#endif
#ifndef BAPS_FLOAT_SPIN_BOX_HH_INCLUDED
# include "baps_float_spin_box.hh"
#endif


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
  : AddMarkerForm( parent, name, fl ), i_marker_table(marker_table)
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
  int no_rows = i_marker_table->numRows();
  for (int i = 0; i < no_rows; ++i)
    {
      // text field == "0.xxx" where xxx is the int we want
      int row_pos = i_marker_table->text(i,0).mid(2).toInt();
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
  int pos = i_trait_pos->value();
  if (!check_position(pos)) return;

  i_marker_table->insertRows(0);

  i_marker_table->setText(0,0,QString("").sprintf("0.%03d",pos));
  i_marker_table->setText(0,1,"trait");
  i_marker_table->setText(0,2,i_trait_low_freq->text());
  i_marker_table->setText(0,3,i_trait_high_freq->text());
  i_marker_table->setText(0,4,"");

  i_marker_table->sortColumn(0,true,true);

  i_trait_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::add_snp()
{
  int pos = i_snp_pos->value();
  if (!check_position(pos)) return;

  i_marker_table->insertRows(0);

  i_marker_table->setText(0,0,QString("").sprintf("0.%03d",pos));
  i_marker_table->setText(0,1,"snp");
  i_marker_table->setText(0,2,i_snp_low_freq->text());
  i_marker_table->setText(0,3,i_snp_high_freq->text());
  i_marker_table->setText(0,4,"");

  i_marker_table->sortColumn(0,true,true);

  i_snp_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::add_ms()
{
  int pos = i_ms_pos->value();
  if (!check_position(pos)) return;

  i_marker_table->insertRows(0);

  i_marker_table->setText(0,0,QString("").sprintf("0.%03d",pos));
  i_marker_table->setText(0,1,"ms");
  i_marker_table->setText(0,2,"");
  i_marker_table->setText(0,3,"");
  i_marker_table->setText(0,4,i_ms_size->text());
  i_marker_table->setText(0,5,i_ms_mutation_rate->text());

  i_marker_table->sortColumn(0,true,true);

  i_ms_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::next_pos_changed(int pos)
{
  i_next_pos->setText(QString("").sprintf("0.%03d",pos));
  i_trait_pos->setValue(pos);
  i_snp_pos  ->setValue(pos);
  i_ms_pos   ->setValue(pos);
}


int AddMarkerImpl::update_next_position(int pos)
{
  int cur_next_pos = i_next_pos_slider->value();
  if (pos != cur_next_pos) return cur_next_pos;

  int step = i_next_pos_step->value();
  int next_pos = cur_next_pos + step;

  // move position -- this automatically updates all that depends on it.
  i_next_pos_slider->setValue(next_pos);

  return next_pos;
}
