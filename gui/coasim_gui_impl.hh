#ifndef COASIMGUIIMPL_H
#define COASIMGUIIMPL_H
#include "coasim_gui_form.h"

class CoasimGuiImpl : public CoasimGuiForm
{
  Q_OBJECT

public:
  CoasimGuiImpl( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
  ~CoasimGuiImpl();

  virtual void add_marker();
  virtual void simulate();
};

#endif // COASIMGUIIMPL_H
