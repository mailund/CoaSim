#ifndef ADD_MARKER_IMPL_H
#define ADD_MARKER_IMPL_H
#include "addmarkerform.h"

class QTable;

class AddMarkerImpl : public AddMarkerForm
{
  Q_OBJECT
  QTable *_marker_table;

  // returns true if it is allowed to add the position, false
  // otherwise.  Shows feedback to the user.
  bool check_position(double pos) const;

  // if pos is the current "next position", increase next position,
  // otherwise do nothing.  Return the new position pos should be set
  // to.
  double update_next_position(double pos);

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
