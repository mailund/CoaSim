#ifndef COASIMGUIIMPL_H
#define COASIMGUIIMPL_H
#include "coasim_gui_form.h"
#include "sim_feedback_impl.hh"

class CoasimGuiImpl : public CoasimGuiForm
{
  Q_OBJECT
  SimFeedbackImpl _monitor;

  

public:
  CoasimGuiImpl(QWidget* parent = 0, const char* name = 0, WFlags fl = 0);
  ~CoasimGuiImpl();

  virtual void add_marker();
  virtual void delete_marker();
  virtual void simulate();
};

#endif // COASIMGUIIMPL_H
