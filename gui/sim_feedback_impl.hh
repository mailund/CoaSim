#ifndef SIM_FEEDBACK_IMPL_H
#define SIM_FEEDBACK_IMPL_H
#include "simfeedbackform.h"

class SimFeedbackImpl : public SimFeedbackForm
{
  Q_OBJECT

public:
  SimFeedbackImpl(QWidget* parent = 0, const char* name = 0, WFlags fl = 0);
  ~SimFeedbackImpl();
};

#endif
