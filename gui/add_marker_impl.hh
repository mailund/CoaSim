#ifndef ADD_MARKER_IMPL_HH_INCLUDED
#define ADD_MARKER_IMPL_HH_INCLUDED

#ifndef ADDMARKERFORM_H_INCLUDED
# include "addmarkerform.h"
# define ADDMARKERFORM_H_INCLUDED
#endif

class QTable;

class AddMarkerImpl : public AddMarkerForm
{
  Q_OBJECT
  QTable *i_marker_table;

  // NB: the positions below are in the integer coordinates -- to get
  // the "real" coordinates, divide by 1000

  // returns true if it is allowed to add the position, false
  // otherwise.  Shows feedback to the user.
  bool check_position(int pos) const;

  // if pos is the current "next position", increase next position,
  // otherwise do nothing.  Return the new position pos should be set
  // to.
  int update_next_position(int pos);

public:
  AddMarkerImpl( QTable *marker_table,
		 QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
  ~AddMarkerImpl();

  virtual void add_trait();
  virtual void add_snp();
  virtual void add_ms();

  virtual void next_pos_changed(int);
};

#endif
