#ifndef COASIM_GUI_IMPL_HH_INCLUDED
#define COASIM_GUI_IMPL_HH_INCLUDED

#ifndef COASIM_GUI_FORM_H_INCLUDED
# include "coasim_gui_form.h"
# define COASIM_GUI_FORM_H_INCLUDED
#endif
#ifndef SIM_FEEDBACK_IMPL_HH_INCLUDED
# include "sim_feedback_impl.hh"
#endif

class CoasimGuiImpl : public CoasimGuiForm
{
  Q_OBJECT
  SimFeedbackImpl _monitor;

public:
  CoasimGuiImpl(QWidget* parent = 0, const char* name = 0, WFlags fl = 0);
  ~CoasimGuiImpl();

  // slots
  virtual void add_marker();
  virtual void delete_marker();
  virtual void simulate();
};

#endif // COASIMGUIIMPL_H
