#include "add_marker_impl.hh"

#include <qtable.h>
#include <qlineedit.h>
#include <qspinbox.h>
#include <qmessagebox.h>
#include <qslider.h>

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

bool AddMarkerImpl::check_position(double pos) const
{
  // since we know that the gui will not allow us to input positions
  // outside the interval [0.0,1.0), we do not check for this.

  // FIXME: if this turns out to be too slow, use binary search -- we
  // know the positions are sorted.
  int no_rows = _marker_table->numRows();
  for (int i = 0; i < no_rows; ++i)
    {
      double row_pos = _marker_table->text(i,0).toDouble();
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


// FIXME: this scale factor is hard-wired for now; perhaps it
// shouldn't be...
static const int POSITION_SCALE_FACTOR = 1000;


void AddMarkerImpl::add_trait()
{
  double pos = _trait_pos->text().toDouble();
  if (!check_position(pos)) return;

  _marker_table->insertRows(0);

  _marker_table->setText(0,0,_trait_pos->text());
  _marker_table->setText(0,1,"trait");
  _marker_table->setText(0,2,_trait_low_freq->text());
  _marker_table->setText(0,3,_trait_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);

  _trait_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::add_snp()
{
  double pos = _snp_pos->text().toDouble();
  if (!check_position(pos)) return;

  _marker_table->insertRows(0);

  _marker_table->setText(0,0,_snp_pos->text());
  _marker_table->setText(0,1,"snp");
  _marker_table->setText(0,2,_snp_low_freq->text());
  _marker_table->setText(0,3,_snp_high_freq->text());
  _marker_table->setText(0,4,"");

  _marker_table->sortColumn(0,true,true);

  _snp_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::add_ms()
{
  double pos = _ms_pos->text().toDouble();
  if (!check_position(pos)) return;

  _marker_table->insertRows(0);

  _marker_table->setText(0,0,_ms_pos->text());
  _marker_table->setText(0,1,"ms");
  _marker_table->setText(0,2,"");
  _marker_table->setText(0,3,"");
  _marker_table->setText(0,4,_ms_size->text());

  _marker_table->sortColumn(0,true,true);

  _ms_pos->setValue(update_next_position(pos));
}

void AddMarkerImpl::next_pos_changed(int pos)
{
  double real_pos = double(pos) / POSITION_SCALE_FACTOR;
  _next_pos->setText(QString("%1").arg(real_pos));

  // WARNING: this only works 'cause the integer positions in the slider
  // and the spin-boxes have the same interpretation
  _trait_pos->setValue(pos);
  _snp_pos  ->setValue(pos);
  _ms_pos   ->setValue(pos);
}

int AddMarkerImpl::update_next_position(double pos)
{
  double cur_next_pos = _next_pos->text().toDouble();
  if (pos != cur_next_pos) return int(cur_next_pos*POSITION_SCALE_FACTOR);

  double step = _next_pos_step->text().toDouble();

  int int_cur_next_pos = int(POSITION_SCALE_FACTOR*cur_next_pos);
  int int_step         = int(POSITION_SCALE_FACTOR*step);
  int int_pos = int_cur_next_pos + int_step;

  // move position -- this automatically updates all that depends on it.
  _next_pos_slider->setValue(int_pos);

  return int_pos;
}
