#ifndef ADD_MARKER_IMPL_H
#define ADD_MARKER_IMPL_H
#include "addmarkerform.h"

class QTable;

class AddMarkerImpl : public AddMarkerForm
{
  Q_OBJECT
  QTable *_marker_table;

public:
  AddMarkerImpl( QTable *marker_table,
		 QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
  ~AddMarkerImpl();

  virtual void add_trait();
  virtual void add_snp();
  virtual void add_ms();
};

#endif
